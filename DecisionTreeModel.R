#PHASE1: DATA CLEANING
#install.packages("caTools")
#library(caTools)

# reading file
loanStats<-read.csv("LoanStats.csv",header = TRUE,stringsAsFactors=FALSE)
loanStats2<-read.csv("LoanStats2.csv",header = TRUE,stringsAsFactors=FALSE)
loan<-rbind(loanStats,loanStats2)

loan$loan_decision<-0
loan$loan_decision[loan$loan_status=="Current"]<-1
loan$loan_decision[loan$loan_status=="Fully Paid"]<-1
loan$loan_decision[loan$loan_status=="In Grace Period"]<-1

#checking the balance of the table
table(loan$loan_decision)
table(loan$loan_decision[loan$loan_decision==0])/table(loan$loan_decision[loan$loan_decision==1])

#fixing skewness of data
loan1<-subset(loan,loan_decision==1)
loan2<-subset(loan,loan_decision==0)

summary(loan1)
summary(loan2)

#install.packages("caTools")
library(caTools)
set.seed(1000)
split = sample.split(loan1, SplitRatio = 0.5)
loan3 = subset(loan1, split==TRUE)
loan<-rbind(loan3,loan2)
table(loan$loan_decision[loan$loan_decision=="NO"])/table(loan$loan_decision[loan$loan_decision=="YES"])
#24% negative values for dependent variable

#creating "verified" column
loan$verification<-"Verified"
loan$verification[loan$verification_status=="Not Verified"]<-"Not Verified"

# removing "months" from term column 
loan$term <- gsub("months", "", loan$term)

#removing "%" from revol_util and int_rate
loan$revol_util<-gsub("%", "", loan$revol_util)
loan$int_rate<-gsub("%", "", loan$int_rate)

#removing "years","year" from "emp_lenght". Additionaly removing "+","<" since aim to classify employment years in groups.
loan$emp_length<-gsub("\\D", "", loan$emp_length)

#converting columns to date format
loan$earliest_cr_line<-as.Date(loan$earliest_cr_line,"%m/%d/%Y")
loan$issue_d<-as.Date(loan$issue_d,"%m/%d/%Y")

#adding "credit_line_history" column
loan$credit_line_history<-loan$issue_d-loan$earliest_cr_line

#creating column "inc_level"
#Rename column name from "emp_title" to "inc_level"
names(loan)[names(loan)=="emp_title"] <- "inc_level"
#setting annual_inc = "inc => 100000" as "High Income"
loan$inc_level[loan$annual_inc > 100000] <- "High Income"
#setting annual_inc = "50000 < inc < 100000" as "Mid Income"
loan$inc_level[50000 < loan$annual_inc &  loan$annual_inc < 100001] <- "Mid Income"
#setting annual_inc = "inc =< 50000" as "Low Income"
loan$inc_level[loan$annual_inc < 50001] <- "Low Income"

#filling emp_lenght blank rows with mean
loan$emp_length<-as.numeric(loan$emp_length)
loan$emp_length[is.na(loan$emp_length)]<-mean(loan$emp_length,na.rm=TRUE)
loan$emp_length<-round(loan$emp_length,digits=0)

#creating "emp_length_category"."1" for 1 year and below, 2 for 2-5 years, "3" for 6-9 years, "4" for 10 and above
loan$emp_length_category<-4
loan$emp_length_category[loan$emp_length==1]<-1
loan$emp_length_category[loan$emp_length>1 & loan$emp_length<6]<-2
loan$emp_length_category[loan$emp_length>5 & loan$emp_length<10]<-3

#creating loan_category column
sort(-table(loan$title))[1:20]
loan$loan_category<-"other"
loan$loan_category[grepl("home", loan$title, ignore.case=TRUE)] <- "Home"
loan$loan_category[grepl("credit card", loan$title, ignore.case=TRUE)] <- "Credit card"
loan$loan_category[grepl("medical", loan$title, ignore.case=TRUE)] <- "Medical"
loan$loan_category[grepl("vacation", loan$title, ignore.case=TRUE)] <- "Vacation"
loan$loan_category[grepl("business", loan$title, ignore.case=TRUE)] <- "Business"
loan$loan_category[grepl("car ", loan$title, ignore.case=TRUE)] <- "Car"
loan$loan_category[grepl("consolidation", loan$title, ignore.case=TRUE)] <- "Debt Consolidation"
loan$loan_category[grepl("refinance", loan$title, ignore.case=TRUE)] <- "Refinance"
loan$loan_category[grepl("purchase", loan$title, ignore.case=TRUE)] <- "Major purchase"
loan$loan_category[grepl("relocation", loan$title, ignore.case=TRUE)] <- "Moving and relocation"
loan$loan_category[grepl("personal", loan$title, ignore.case=TRUE)] <- "Personal loan"

#filling revol_util blank rows with mean
loan$revol_util<-as.numeric(loan$revol_util)
loan$revol_util[is.na(loan$revol_util)]<-mean(loan$revol_util,na.rm=TRUE)

#converting variables into factors
loan$loan_category<-as.factor(loan$loan_category)
loan$verification<-as.factor(loan$verification)
loan$inc_level<-as.factor(loan$inc_level)
loan$emp_length_category<-as.factor(loan$emp_length_category)

#remove variables which are not suitable for the model
loan$verification_status<- NULL
loan$title<- NULL
loan$purpose<- NULL
loan$earliest_cr_line <-NULL
loan$loan_status<-NULL

loan$grade<-NULL
loan$sub_grade<-NULL	
loan$inc_level<-NULL
loan$home_ownership<-NULL
loan$issue_d<-NULL
loan$pymnt_plan<-NULL	
loan$addr_state<-NULL
loan$revol_util<-NULL
loan$initial_list_status<-NULL
loan$total_rec_prncp<-NULL	
loan$total_rec_int<-NULL	
loan$total_rec_late_fee<-NULL	
loan$recoveries<-NULL	
loan$collection_recovery_fee<-NULL
loan$verification<-NULL
loan$loan_category<-NULL
loan$dti<-NULL
loan$int_rate<-NULL

#remove variables to reduce noise
loan$zip_code<- NULL
loan$desc<- NULL
loan$mths_since_last_delinq<- NULL
loan$mths_since_last_record<- NULL
loan$last_pymnt_d<- NULL
loan$next_pymnt_d<- NULL
loan$mths_since_last_major_derog<- NULL
loan$last_credit_pull_d<- NULL

#write.csv(loan, file="modLoan2.csv")

#decision tree model

#Install packages 
#install.packages("tree")
#install.packages("rpart")
#install.packages("rattle")

#Importing packages
library(tree)
library(rpart)
library(rattle)

#Using split to create train/test set
set.seed(2)
split1 = sample.split(loan, SplitRatio = 0.7)
train = subset(loan, split1==TRUE)
test= subset(loan, split1==FALSE)

#check NA in data
#train[!complete.cases(train),]
#omit NA from train data
#newtrain <- na.omit(train)
#newtrain[!complete.cases(newtrain),]

#fit the tree model using training data
tree_model = rpart(loan_decision~., train)
#tree_model = tree(loan_decision~., train)

#Plotting the result using tree model
plot(tree_model)
text(tree_model, pretty=0)
fancyRpartPlot(tree_model, cex=0.5)

#Summarize what the model contains
summary(tree_model)

#check how the model is doing using the test data
tree_pred = predict(tree_model, test)
#If prediction is bigger than 0.65, decision is 1(yes) if not, 0 (No)
tree_pred = ifelse(tree_pred > 0.65, 1, 0) 
table(tree_pred)
mean(tree_pred != test$loan_decision) #6.6% error

#Display cp table
printcp(tree_model)	

#Use table to see the correctness of the prediction based on the range of >0.65
table(test$loan_decision, tree_pred>0.65)
(56243+10001)/nrow(test)*100 #93.4% correctness

###Prune the tree

##cross validation to check where to stop pruning
set.seed(3)
tree_model2 = tree(loan_decision~., train)
cv_tree = cv.tree(tree_model2)
#warnings(cv_tree)
names(cv_tree)
plot(cv_tree$size, cv_tree$dev, type ="b")

###check how it is doing
pruned_model = prune.tree(tree_model2, best=8)
plot(pruned_model)
text(pruned_model, pretty=0)

tree_pred2 = predict(pruned_model, test)
tree_pred2 = ifelse(tree_pred2 > 0.65, 1, 0)
table(tree_pred2)
mean(tree_pred2 != test$loan_decision) #7.2% error

table(test$loan_decision, tree_pred2>0.65)
(55693+10153)/nrow(test)*100 #92.8% correctness
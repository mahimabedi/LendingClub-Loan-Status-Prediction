##install.packages should be run only first time


###PHASE1: DATA CLEANING

#install.packages("caTools")
library(caTools)

# reading and merging files
loanStats<-read.csv("LoanStats.csv",header = TRUE,stringsAsFactors=FALSE)
loanStats2<-read.csv("LoanStats2.csv",header = TRUE,stringsAsFactors=FALSE)
loan<-rbind(loanStats,loanStats2)

#creating "loan_decision" as dependent variable
loan$loan_decision<-0
loan$loan_decision[loan$loan_status=="Current"]<-1
loan$loan_decision[loan$loan_status=="Fully Paid"]<-1
loan$loan_decision[loan$loan_status=="In Grace Period"]<-1

#checking balance of data
table(loan$loan_decision)
table(loan$loan_decision[loan$loan_decision==0])/table(loan$loan_decision[loan$loan_decision==1])
#11% positive values

#fixing skewness of data
loan1<-subset(loan,loan_decision==1)
loan2<-subset(loan,loan_decision==0)
set.seed(1000)
split = sample.split(loan1, SplitRatio = 0.3)
loan3 = subset(loan1, split==TRUE)
loan<-rbind(loan3,loan2)
table(loan$loan_decision[loan$loan_decision==0])/table(loan$loan_decision[loan$loan_decision==1])
#40% positive values

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

#filling emp_lenght blank rows with mean
loan$emp_length<-as.numeric(loan$emp_length)
loan$emp_length[is.na(loan$emp_length)]<-mean(loan$emp_length,na.rm=TRUE)
loan$emp_length<-round(loan$emp_length,digits=0)

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

#converting variables into factors/numeric
loan$loan_category<-as.factor(loan$loan_category)
loan$verification<-as.factor(loan$verification)
loan$home_ownership<-as.factor(loan$home_ownership)
loan$grade<-as.factor(loan$grade)
loan$sub_grade<-as.factor(loan$sub_grade)

#remove variables from which other variabes have been made
loan$verification_status<- NULL
loan$title<- NULL
loan$purpose<- NULL
loan$earliest_cr_line <-NULL
loan$loan_status<-NULL

#remove variables to reduce noise
loan$zip_code<- NULL
loan$desc<- NULL
loan$mths_since_last_delinq<- NULL
loan$mths_since_last_record<- NULL
loan$last_pymnt_d<- NULL
loan$next_pymnt_d<- NULL
loan$mths_since_last_major_derog<- NULL
loan$last_credit_pull_d<- NULL
loan$tot_coll_amt<-NULL
loan$tot_cur_bal<-NULL
loan$total_credit_rv<-NULL
loan$grade<-NULL
loan$addr_state<-NULL


###PHASE2: MODELS

#install.packages("randomForest")
#install.packages("ROCR")
library(randomForest)
library(ROCR)

#spliting into training ang testing dataset
set.seed(1000)
split1 = sample.split(loan, SplitRatio = 0.7)
train = subset(loan, split1==TRUE)
test= subset(loan, split1==FALSE)

#RF model removing least impoartant variables (emp_length,verification,grade,delinq_2yrs)
M8<-randomForest(loan_decision~inq_last_6mths+pub_rec+dti+loan_category+annual_inc+sub_grade+term+revol_util+home_ownership,data=train,ntree=20,importance=TRUE)

#variable importance plot(please see plot)
varImpPlot(M8,main="RF Model 8 Variable Importance",col="blue",pch=19)

#training set predictions
predM8train<-predict(M8,newdata=train)
#sum of squared errors
SSE<-sum((predM8train-train$loan_decision)^2)
#baseline errors
SST<-sum((1-train$loan_decision)^2)


#test set predictions
predM8test<-predict(M8,newdata=test)
#sum of squared errors
SSE<-sum((predM8test-test$loan_decision)^2)
#baseline errors
SST<-sum((1-test$loan_decision)^2)
((SST-SSE)/SST)*100


#tuning the mtry parameters and increase the ntree to 500 for final model.
x<-data.frame(train[,4],train[,7],train[,10],train[,11],train[,15],train[,17],train[,21],train[,19],train[,36])
mtry <- tuneRF(x[1:8], x[,9])
#mtry=2 (please see plot)

#final RF model with tuned parameters
M9<-randomForest(loan_decision~inq_last_6mths+pub_rec+dti+loan_category+annual_inc+sub_grade+term+revol_util+home_ownership,data=train,ntree=500,mtry=2,importance=TRUE)

#variable importance plot
varImpPlot(M9,main="RF Model 9 Variable Importance",col="blue",pch=19)

#log-log plot of OOB errors and ntrees (please see plot)
plot(M9, log="x")

#predictions on training set
predM9train<-predict(M9,newdata=train)
SSE<-sum((predMtrain-train$loan_decision)^2)
SST<-sum((1-train$loan_decision)^2)
(SST-SSE)/SST*100


#prediction on testing set
predM9test<-predict(M9,newdata=test,type="class")
#sum of squared errors
SSE<-sum((predM9test-test$loan_decision)^2)
#baseline errors
SST<-sum((1-test$loan_decision)^2)
(SST-SSE)/SST*100


#Model accuracy-using higher threshold of 0.6 since aim is to reduce the false positives
table(test$loan_decision,predM9test>0.65)
(28415+6770)/nrow(test)*100


#prediction probabilities
pred2 <- predict(M9, newdata=test,type="response")
preds <- prediction(as.numeric(pred2), test$loan_decision)
perf <- performance(preds,"tpr","fpr")

#AUC plot (please see plot)
plot(perf,col='blue',lwd=3)
abline(a=0,b=1,lwd=2,lty=2,col="green")

#AUC absoulte number
as.numeric(performance(preds,"auc")@y.values)


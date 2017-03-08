rm(list=ls(all=TRUE))

train <- read.csv("C:/Users/Anoop/Desktop/Loan Pred III/train_AV.csv",na.strings = "",header=T)
test <- read.csv("C:/Users/Anoop/Desktop/Loan Pred III/test_AV.csv",na.strings = "",header=T)

#removing loan ID
train <- train[,-1]
loanID <- as.data.frame(test[,1])
test <- test[,-1]


str(train)
summary(train)
str(test)
summary(test)

# converting attr to factors
train$Credit_History <- as.factor(train$Credit_History)
test$Credit_History <- as.factor(test$Credit_History)



z <- train$Credit_History == 1
#table(z)["TRUE"]
sum(z, na.rm=TRUE)


# KNN Imputation
library(DMwR)

train_data<-knnImputation(train,scale=T,k=5) #KNN Imputation
sum(is.na(train_data))

X_test<-knnImputation(test,scale=T,k=5) #KNN Imputation
sum(is.na(X_test))

#data manupuation

levels(train_data$Dependents)
levels(train_data$Dependents)[levels(train_data$Dependents)=="3+"] <- "3"
levels(X_test$Dependents)[levels(X_test$Dependents)=="3+"] <- "3"

train_data$Dependents <- as.integer(as.character(factor(train_data$Dependents)))
X_test$Dependents <- as.integer(as.character(factor(X_test$Dependents)))

summary(train_data)
str(train_data)

summary(X_test)
str(X_test)

# converting Gender to binary
train_data$Gender <- ifelse(train_data$Gender == "F", 1, 0)
X_test$Gender <- ifelse(X_test$Gender == "F", 1, 0)

# converting Married to binary
train_data$Married <- ifelse(train_data$Married == "No", 1, 0)
X_test$Married <- ifelse(X_test$Married == "No", 1, 0)

# converting Education to binary
train_data$Education <- ifelse(train_data$Education == "Not Graduate", 1, 0)
X_test$Education <- ifelse(X_test$Education == "Not Graduate", 1, 0)

# converting Self_Employed to binary
train_data$Self_Employed <- ifelse(train_data$Self_Employed == "No", 1, 0)
X_test$Self_Employed <- ifelse(X_test$Self_Employed == "No", 1, 0)

# converting Credit_History to binary
train_data$Credit_History <- ifelse(train_data$Credit_History == "0", 1, 0)
X_test$Credit_History <- ifelse(X_test$Credit_History == "0", 1, 0)

# creating dummmies for the Property_Area
library(dummies)
train_data <- dummy.data.frame(train_data, names=c("Property_Area"), sep="_")
X_test <- dummy.data.frame(X_test, names=c("Property_Area"), sep="_")

Data_NumAtr <- subset(train_data,select=c(ApplicantIncome,CoapplicantIncome,LoanAmount,Loan_Amount_Term,Dependents))
Data_CatAtr <- subset(train_data,select=-c(ApplicantIncome,CoapplicantIncome,LoanAmount,Loan_Amount_Term,Dependents))

Data_NumAtr_test <- subset(X_test,select=c(ApplicantIncome,CoapplicantIncome,LoanAmount,Loan_Amount_Term,Dependents))
Data_CatAtr_test <- subset(X_test,select=-c(ApplicantIncome,CoapplicantIncome,LoanAmount,Loan_Amount_Term,Dependents))

#  standardizing data
library(vegan)
#Using range method
#dataStd. <- decostand(Data_NumAtr,"range") 
#summary(dataStd.)

#Using Z score method #train
dataStd. <- decostand(Data_NumAtr,"standardize")
summary(dataStd.)

Data_NumAtr_1 <- dataStd. 

#Using Z score method #test

dataStd_test <- decostand(Data_NumAtr_test,"standardize")
summary(dataStd_test)
  
Data_NumAtr_test_1 <- dataStd_test 

  
dataForModel <- cbind(Data_NumAtr_1,Data_CatAtr)
dataForTest <- cbind(Data_NumAtr_test_1,Data_CatAtr_test)

#Split the data into train and test data sets
#rows=seq(1,nrow(dataForModel),1)
#set.seed(123)
#trainRows=sample(rows,(70*nrow(dataForModel))/100)
#train = dataForModel[trainRows,] 
#test = dataForModel[-trainRows,]

# Build linear regression and interpret the results
#Input attributes by selection
#LinReg<-lm(TotalRevenueGenerated~City+NoOfChildren+ Tenure+NoOfUnitsPurchased, data=dataForModel)
#summary(LinReg)
#Input all attributes into model 
#LinReg1<- lm( ~ ., data=dataForModel)
#summary(LinReg1)
# compare the models with limited atttributes and all attributes
#summary(LinReg)
#summary(LinReg1)


#library(randomForests)
#hepatitis_rf <- randomForest(target ~ ., data=trainR, ntree=30,mtry = 4)

#LogReg <- glm(Loan_Status ~ Credit_History+Property_Area+Married+Gender+LoanAmount, data=train, family=binomial) 
#summary(LogReg)

# Boosting Model
library(ada)
x = subset(dataForModel, select = -Loan_Status)
y = as.factor(dataForModel$Loan_Status)
a = dataForTest
#a = subset(dataForTest, select = -Loan_Status)
#b = as.factor(dataForTest$Loan_Status)

model=ada(x, y, iter=25, loss="logistic") #20 Iterations

# Train results
pred<-predict(model,x) 
pred
result_train <- table(pred, y)
#pred_class <- factor(ifelse(prob> 0.5, 1, 0)) 
#conf.mat <- table(train$Loan_Status,pred_class)

accuracy_train <- sum(diag(result_train))/sum(result_train)
accuracy_train

# Test results
pred_test<-predict(model,a) 
pred_test

LoanStatus_test <- as.data.frame(pred_test)
names(LoanStatus_test) <- "Loan_Status"
names(loanID) <- "Loan_ID"
submit_loan <- cbind(loanID,LoanStatus_test)
write.csv(submit_loan, "C:/Users/Anoop/Desktop/Loan Pred III/submit.csv", row.names=F)

#result_test <- table(pred_test, b)
#pred_class <- factor(ifelse(prob> 0.5, 1, 0)) 
#conf.mat <- table(train$Loan_Status,pred_class)

#accuracy_test <- sum(diag(result_test))/sum(result_test)
#accuracy_test

#Multicollinearity check 
#library(car) 
#vif(LogReg)
plot(model)

setwd("C:/Users/DELL-PC/Downloads")
train=read.csv("train_u6lujuX_CVtuZ9i.csv")
test=read.csv("test_Y3wMUE5_7gLdaTN.csv")

#Treating the missing values
test$Loan_Amount_Term[which(is.na(test$Loan_Amount_Term)==TRUE)]=median(test$Loan_Amount_Term,na.rm=TRUE)
train$Loan_Amount_Term[which(is.na(train$Loan_Amount_Term)==TRUE)]=median(train$Loan_Amount_Term,na.rm=TRUE)
train$Credit_History[which(is.na(train$Credit_History)==TRUE)]=median(train$Credit_History,na.rm=TRUE)
test$Credit_History[which(is.na(test$Credit_History)==TRUE)]=median(test$Credit_History,na.rm=TRUE)
train$LoanAmount[which(is.na(train$LoanAmount)==TRUE)]=median(train$LoanAmount,na.rm=TRUE)
test$LoanAmount[which(is.na(test$LoanAmount)==TRUE)]=median(test$LoanAmount,na.rm=TRUE)


#Changing the independent factor variable into numeric
train$Married=as.numeric(train$Married)
test$Married=as.numeric(test$Married)

#Changing the independent factor variable into numeric
train$Gender=as.numeric(train$Gender)
test$Gender=as.numeric(test$Gender)

#Changing the independent factor variable into numeric
train$Self_Employed=as.numeric(train$Self_Employed)
test$Self_Employed=as.numeric(test$Self_Employed)

#Changing the independent factor variable into numeric
train$Education=as.numeric(train$Education)
test$Education=as.numeric(test$Education)

#Changing the independent factor variable into numeric
train$Property_Area=as.numeric(train$Property_Area)
test$Property_Area=as.numeric(test$Property_Area)

#Changing the dependent factor variable into numeric
train$Loan_Status=as.numeric(train$Loan_Status)

#Builiding the model
model2=lm(Loan_Status ~ Married + Education + CoapplicantIncome +Credit_History, data = train)
summary(model2)

#predicting 
pred=predict(model4,newdata=test)

#Forming new dataset after prediction
Loan_ID=c(numeric(nrow(test)))
Loan_ID=test$Loan_ID
Loan_Status=pred
plot(pred)
Loan_Status[which(Loan_Status>0.1)]="Y"
Loan_Status[which(Loan_Status<=0.1)]="N"
Sub=data.frame(Loan_ID,Loan_Status)

write.csv(Sub,file="Sub1.csv")

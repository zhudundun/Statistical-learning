train=read.csv("train.csv")
test=read.csv("test.csv")
loan_status=train$loan_status
train=subset(train,select=-loan_status)
data=rbind(train,test)


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#replace NA with median or mode#
revalue=function(x) {
  if(is.factor(x)) replace(x, is.na(x), Mode(na.omit(x)))
  else replace(x, is.na(x), median(x, na.rm=TRUE))
}




fico_average=apply(subset(data,select=c(fico_range_low,fico_range_high)),1,mean)
data=cbind(fico_average,data)

term.num=ifelse(data$term=="60 months",60,36)
data=cbind(term.num,data)



#change y into 0,1#
levels(loan_status)
loan.yesno=ifelse(loan_status=="Fully Paid",0,1)


#extraxt year info from earliest_credit_line#
library(lubridate)
credit.line=as.Date(x=paste("1-",data$earliest_cr_line,sep=""),"%d-%b-%Y")
credit.year=year(credit.line)
data=cbind(credit.year,data)

#remove some column#
data=subset(data,select=-c(grade,title,emp_title,zip_code,earliest_cr_line,term,fico_range_low,fico_range_high))



n=nrow(train)
y=data.frame(cbind(data[1:n,]$id,loan.yesno))





train.new=data[1:n,]
test.new=data[(n+1):nrow(data),]

Missing=rep('x',ncol(train.new))
for (i in 1:ncol(train.new)){Missing[i]=any(is.na(train.new[,i]))}

Missingcol=which(Missing[]==TRUE)


for (i in Missingcol){train.new[,i]=revalue(train.new[,i])}
for (i in Missingcol){test.new[,i]=revalue(test.new[,i])}


glm.model = glm(y$loan.yesno ~ .-id, train.new , family = binomial(link = 'logit'))
pred1 = predict(glm.model , test.new , type = 'response')

library(glmnet)
train.lasso=data.matrix(subset(train.new,select=-id))
test.lasso=data.matrix(subset(test.new,select=-id))

lassomodel<- glmnet(train.lasso, y$loan.yesno, alpha = 1,family="binomial")
cv.out <- cv.glmnet(train.lasso, y$loan.yesno, alpha = 1,family="binomial")
pred2 = predict(lassomodel,s=cv.out$lambda.min, newx=test.lasso,type="response")

library(xgboost)
boostmodel<- xgboost(data=train.lasso, label=y$loan.yesno,nrounds=169, eta=0.1,objective = "binary:logistic")
#nrounds=45, eta=0.5#
pred3 <- predict(boostmodel, test.lasso)

pred1=cbind(test$id,pred1)
pred2=cbind(test$id,pred2)
pred3=cbind(test$id,pred3)

colnames(pred2) <- c("id","prob")
colnames(pred1) <- c("id","prob")
colnames(pred3) <- c("id","prob")




write.table(pred1, "./mysubmission1.txt", sep=",",row.names = FALSE)
write.table(pred2, "./mysubmission2.txt", sep=",",row.names = FALSE)
write.table(pred3, "./mysubmission3.txt", sep=",",row.names = FALSE)


train <- read.csv("./Ames_traindata.csv")
test <- read.csv("./Ames_testdata.csv")

Housing=rbind(train,test)


library("gbm")
library("glmnet")

# subhousign is the data for booosting 
# lsubhousign is the data for lasso
subhousing=Housing[ , -which(names(Housing) %in% c("PID","Sale_Price","Garage_Yr_Blt"))]
lsubhousing=Housing[ , -which(names(Housing) %in% c("PID","Sale_Price","Garage_Yr_Blt","Street","Longitude","Latitude"))]

y.train=train$Sale_Price
y.test=test$Sale_Price
y.train=log(y.train)
y.test=log(y.test)

x=subhousing[1:nrow(train),]
testx=subhousing[(nrow(train)+1):nrow(Housing),]
lx=lsubhousing[1:nrow(train),]
ltestx=lsubhousing[(nrow(train)+1):nrow(Housing),]

# winsorize is a way to treat outliers here
mywinsorize=function(x) { 
  top <- quantile(x,0.95)
  bot <- quantile(x,0.05)
  x[x > top] <- top 
  x[x < bot] <- bot
  return(x) }

# get numerical variables
nums=which(sapply(lx, is.numeric))
for (i in 1:32){lx[,nums[i]]=mywinsorize(lx[,nums[i]])}
for (i in 1:32){ltestx[,nums[i]]=mywinsorize(ltestx[,nums[i]])}

xmatrix<- model.matrix(y.train~ ., data=x)
testmatrix<- model.matrix(y.test~ ., data=testx)
lxmatrix<- model.matrix(y.train~ ., data=lx)
ltestmatrix<- model.matrix(y.test~ ., data=ltestx)




# boosting model
gbm.fit <- gbm(
  formula = y.train ~ .,
  distribution = "gaussian",
  data = data.frame(xmatrix),
  n.trees = 3867,
  interaction.depth = 5,
  shrinkage = 0.01,
  n.minobsinnode = 5,
  bag.fraction = .65, 
  train.fraction = 1,
  n.cores = NULL, 
  verbose = FALSE
)



# lasso model
mylasso = glmnet(lxmatrix, y.train, alpha = 1)   
cvlasso = cv.glmnet(lxmatrix, y.train, alpha = 1)   

pred1 <- predict(gbm.fit, n.trees = gbm.fit$n.trees, data.frame(testmatrix))
pred2 <- predict(mylasso,s=cvlasso$lambda.min, newx = ltestmatrix)

pred1=cbind(test$PID,pred1)
pred2=cbind(test$PID,pred2)
colnames(pred2) <- c("PID","Sale_Price")
colnames(pred1) <- c("PID","Sale_Price")




write.table(pred1, "./mysubmission1.txt", sep=",",row.names = FALSE)
write.table(pred2, "./mysubmission2.txt", sep=",",row.names = FALSE)


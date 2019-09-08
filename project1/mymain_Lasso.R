train <- read.csv("./Ames_traindata.csv")
test <- read.csv("./Ames_testdata.csv")

Housing=rbind(train,test)


remove_column1=c("Street","Longitude","Latitude","Garage_Yr_Blt","PID","Sale_Price")
subHousing=Housing[ , -which(names(Housing) %in% remove_column1)]





y=train$Sale_Price
testy=test$Sale_Price

y=log(y)
testy=log(testy)

mywinsorize=function(x) { 
  top <- quantile(x,0.95)
  bot <- quantile(x,0.05)
  x[x > top] <- top 
  x[x < bot] <- bot
  return(x) }

traindata=subHousing[1:nrow(train),]
testdata=subHousing[(nrow(train)+1):nrow(Housing),]

nums=which(sapply(traindata, is.numeric))
x=matrix(0,nrow(traindata),ncol(traindata))
testx=matrix(0,nrow(testdata),ncol(testdata))

for (i in 1:32){traindata[,nums[i]]=mywinsorize(traindata[,nums[i]])}
for (i in 1:32){testdata[,nums[i]]=mywinsorize(testdata[,nums[i]])}
x=traindata
testx=testdata


xmatrix<- model.matrix(y~ ., data=x)
testmatrix<- model.matrix(testy~ ., data=testx)



scalematrix=scale(xmatrix)
remove_column2=colnames(scalematrix)[colSums(is.na(scalematrix)) > 0]

xmatrix=xmatrix[ , -which(names(data.frame(xmatrix)) %in% remove_column2)]
testmatrix=testmatrix[ , -which(names(data.frame(testmatrix)) %in% remove_column2)]
xmatrix=xmatrix[,-1]
testmatrix=testmatrix[,-1]




mylasso = function(x, y, lam, n.iter = 50,standardize=TRUE)
{ 
  lambda=lam
  p=ncol(x)
  
  sd_x=matrix(0,1,p)
  xba=matrix(0,1,p)
  sd_y=sqrt(var(y))
  yba=mean(y)
  for (j in 1:p) {sd_x[j]=sqrt(var(xmatrix[,j]))}
  for (j in 1:p) {xba[j]=mean(xmatrix[,j])}
  
  
  if (standardize==TRUE) {
    x=scale(x) 
    y=scale(y)
  }
  
  b <- rep(0,p)
  r <- y
  
  for(step in 1:n.iter){for(j in 1:p){
    r <- r + x[,j]*b[j]
    xr <- sum(x[,j]*r)
    xx <- sum(x[,j]^2)   
    b[j] <- (abs(xr)-lambda/2)/xx
    b[j] <- sign(xr)*ifelse(b[j]>0,b[j],0)
    r <- r - x[,j]*b[j]
  }
  }
  for (j in 1:p){b[j]=b[j]*sd_y/sd_x[j]}
  b0=yba-sum(b[j]*xba[j])
  return(c(b0,b))} 

lam=23.33668
cdlasso=mylasso(xmatrix, y, lam, n.iter = 50,standardize=TRUE)
intercept=matrix(1,nrow(testmatrix),1)
testmatrix_inter=cbind(intercept,testmatrix)
ytest.pred = testmatrix_inter%*%cdlasso
RSME=sqrt(mean((ytest.pred- testy)^2))



pred=cbind(Housing[(nrow(train)+1):nrow(Housing),]$PID,ytest.pred)
colnames(pred) <- c("PID","Sale_Price")
write.table(pred, "./mysubmission.txt", sep=",",row.names = FALSE)

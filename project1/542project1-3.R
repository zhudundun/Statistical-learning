Housing <- read.csv("C:/Users/wang/Documents/UIUC/R/Ames_data.csv")

subHousing=Housing[,-82]

subHousing=subHousing[,-81]
which( colnames(x)=="Garage_Yr_Blt")

subHousing=subHousing[,-60]

subHousing=subHousing[,-6]
# subHousing=subHousing[,-76]
# subHousing=subHousing[,-75]
# subHousing=subHousing[,-73]
# subHousing=subHousing[,-72]
# subHousing=subHousing[,-70]
# subHousing=subHousing[,-60]
# subHousing=subHousing[,-46]
# subHousing=subHousing[,-40]
# subHousing=subHousing[,-23]
# subHousing=subHousing[,-15]
# subHousing=subHousing[,-12]
# subHousing=subHousing[,-10]

test.id = seq(1, 2930, by=3)
testdata=subHousing[test.id,]
traindata=subHousing[-test.id,]







x=traindata[,-1]
testx=testdata[,-1]
#nums <- unlist(lapply(x, is.numeric))

#x [,nums]<- scale(x[,nums])

x=x[,-78]
testx=testx[,-78]


y=traindata[,79]
testy=testdata[,79]

y=log(y)
testy=log(testy)
#y<- scale(y)

nums=which(sapply(x, is.numeric))
library("robustHD")
for (i in 1:32){x[,nums[i]]=mywinsorize(x[,nums[i]])}
for (i in 1:32){testx[,nums[i]]=mywinsorize(testx[,nums[i]])}




xmatrix<- model.matrix(y~ ., x)
testmatrix<- model.matrix(testy~ ., data=testx)


colnames(xmatrix)[colSums(is.na(xmatrix)) > 0]
#after scaling some column become NA 

# xmatrix=xmatrix[,-288]
# xmatrix=xmatrix[,-274]
# xmatrix=xmatrix[,-272]
# xmatrix=xmatrix[,-230]
# xmatrix=xmatrix[,-227]
# xmatrix=xmatrix[,-220]
# 
# xmatrix=xmatrix[,-157]
# xmatrix=xmatrix[,-150]
# xmatrix=xmatrix[,-148]
# 
# xmatrix=xmatrix[,-134]
# xmatrix=xmatrix[,-119]
# 
# xmatrix=xmatrix[,-49]
# xmatrix=xmatrix[,-3]
# xmatrix=xmatrix[,-1]

# b=colnames(a)[colSums(is.na(a)) > 0]
# which( colnames(xmatrix) %in% b )
# give the column name that become sd0 after scale

xmatrix=xmatrix[,-230]


xmatrix=xmatrix[,-157]
xmatrix=xmatrix[,-150]
xmatrix=xmatrix[,-148]

xmatrix=xmatrix[,-134]
xmatrix=xmatrix[,-119]

xmatrix=xmatrix[,-49]
xmatrix=xmatrix[,-3]
xmatrix=xmatrix[,-1]
#remove the column with 0 variance

# testmatrix=testmatrix[,-288]
# testmatrix=testmatrix[,-274]
# testmatrix=testmatrix[,-272]
# 
# testmatrix=testmatrix[,-230]
# testmatrix=testmatrix[,-227]
# testmatrix=testmatrix[,-220]
# 
# 
# testmatrix=testmatrix[,-157]
# testmatrix=testmatrix[,-150]
# testmatrix=testmatrix[,-148]
# 
# testmatrix=testmatrix[,-134]
# testmatrix=testmatrix[,-119]
# 
# testmatrix=testmatrix[,-49]
# testmatrix=testmatrix[,-3]
# testmatrix=testmatrix[,-1]




testmatrix=testmatrix[,-230]



testmatrix=testmatrix[,-157]
testmatrix=testmatrix[,-150]
testmatrix=testmatrix[,-148]

testmatrix=testmatrix[,-134]
testmatrix=testmatrix[,-119]

testmatrix=testmatrix[,-49]
testmatrix=testmatrix[,-3]
testmatrix=testmatrix[,-1]








#xmatrix=scale(xmatrix)




 
 
  lamsequence  <- exp(seq(4,8,length=200))

 
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
  
 
 # pct <- rowSums(abs(beta_cd))/sum(abs(beta_cd[1,]))
 # matplot(pct,beta_cd,type="l",lty=1,xlab="|beta|/max(|beta|)",ylab="Coefficients")
 #  text(1.02,beta_cd[1,],1:p,cex=1,col=1:p)
 #  
 #
 cdlasso=mylasso(xmatrix, y, lamdba, n.iter = 50,standardize=TRUE)
  #ytest.pred=matrix(0,nlambda,nrow(testmatrix))
  intercept=matrix(1,nrow(testmatrix),1)
  testmatrix_inter=cbind(intercept,testmatrix)
 
   ytest.pred = testmatrix_inter%*%cdlasso
   RSME=matrix(0,nlambda,1)
   RSME=sqrt(mean((ytest.pred- testy)^2))
   which.min(RSME)
   
  #try the data with built in package glmnet
   mylasso = glmnet(xmatrix, y.train, alpha = 1)   
   cvlasso = cv.glmnet(xmatrix, y.train, alpha = 1)   
   plot(cvlasso)
   Ytest.pred = predict(mylasso, newx = testmatrix)
   sqrt(mean((Ytest.pred - y.test)^2)) 
   
   # try different lambda
   cdlasso=matrix(0,200,291)
    lamsequence  <- (seq(12,60,length=200))
    for (i in 1:200){cdlasso[i,]=mylasso(xmatrix, y, lam=lamsequence[i], n.iter = 50,standardize=TRUE)}
    ytest.pred=matrix(0,200,977)
   for (i in 1:200){ytest.pred[i,]= testmatrix_inter%*%cdlasso[i,]}
   RMSE=matrix(0,200,1)
   for (i in 1:200) {RMSE[i,]=sqrt(mean((ytest.pred[i,] - testy)^2))}
   which.min(RMSE)
   
   mywinsorize=function(x) { 
     top <- quantile(x,0.95)
     bot <- quantile(x,0.05)
     x[x > top] <- top 
     x[x < bot] <- bot
     return(x) }
   
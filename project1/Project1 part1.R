Housing <- read.csv("C:/Users/wang/Documents/UIUC/R/Ames_data.csv")
test.id <- read.csv("C:/Users/wang/Documents/UIUC/R/Project1_test_id.csv")
library("gbm")
library("caret")
mywinsorize=function(x) { 
  top <- quantile(x,0.95)
  bot <- quantile(x,0.05)
  x[x > top] <- top 
  x[x < bot] <- bot
  return(x) }

RMSE=matrix(0,10,2)
for(i in 4:6)
{traindata=subset(Housing, !(PID %in% test.id[,i]))
testdata=subset(Housing, (PID %in% test.id[,i]))

remove_column1=c("Garage_Yr_Blt","PID","Sale_Price")
train=traindata[ , -which(names(traindata) %in% remove_column1)]
test=testdata[ , -which(names(testdata) %in% remove_column1)]

y=traindata[,83]
testy=testdata[,83]
y=log(y)
testy=log(testy)



xmatrix<- model.matrix(y~ ., data=train)
testmatrix<- model.matrix(testy~ ., data=test)

gbm.fit <- gbm(
     formula = y ~ .,
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
pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, data.frame(testmatrix))


RMSE[i,1]=caret::RMSE(pred, testy)

}


# gbm.fit <- gbm(
#   formula = y ~ .,
#   distribution = "gaussian",
#   data = data.frame(xmatrix),
#   n.trees = 3867,
#   interaction.depth = 5,
#   shrinkage = 0.01,
#   n.minobsinnode = 5,
#   bag.fraction = .65, 
#   train.fraction = 1,
#   n.cores = NULL, 
#   verbose = FALSE
# )
# 
# gbm.fit <- gbm(
#   formula = y ~ .,
#   distribution = "gaussian",
#   data = data.frame(xmatrix),
#   n.trees = 483,
#   interaction.depth = 5,
#   shrinkage = 0.1,
#   n.minobsinnode = 5,
#   bag.fraction = .65, 
#   train.fraction = 1,
#   n.cores = NULL, 
#   verbose = FALSE
# )

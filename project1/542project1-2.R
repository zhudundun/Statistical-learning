Housing <- read.csv("C:/Users/wang/Documents/UIUC/R/Ames_data.csv")
# colnames(Housing)[colSums(is.na(Housing)) > 0]
# #[1] "Garage_Yr_Blt" only column with missing value
# which( colnames(Housing)=="Garage_Yr_Blt")
# Housingcomplete=Housing[,-60]

traindata=subset(Housing, !(PID %in% test.id[,3]))
testdata=subset(Housing, (PID %in% test.id[,3]))

remove_column1=c("Street","Longitude","Latitude","Garage_Yr_Blt","PID","Sale_Price")
train=traindata[ , -which(names(traindata) %in% remove_column1)]
test=testdata[ , -which(names(testdata) %in% remove_column1)]


test.id <- read.csv("C:/Users/wang/Documents/UIUC/R/Project1_test_id.csv")

write.csv(traindata, file = "Ames_traindata.csv")
 write.csv(testdata, file = "Ames_testdata.csv")


 
 
 

#   library("randomForest")
#    rfModel = randomForest(y ~ ., data = xmatrix,importance = T, ntree=500)
# #   yhat.test = predict(rfModel, testdata)
# #   y.test=log(testdata[,82])
# #   sqrt(sum((y.test - yhat.test)^2)/length(test.id))
 
 
 

  test.id5=which(Housingcomplete[,1] %in% test.id[,5])
  #boosting
 library("gbm")
  # x=Housingcomplete[,-82]
  # 
  # x=Housingcomplete[,-1]
  # y=Housingcomplete[,82]
  y=traindata[,83]
  testy=testdata[,83]
  y=log(y)
  testy=log(testy)
  
  xmatrix<- model.matrix(y~ ., data=train)
  testmatrix<- model.matrix(testy~ ., data=test)
  # gbm.fit <- gbm(
  #        formula = log(y.train) ~ .,
  #        distribution = "gaussian",
  #        data =data.frame( xmatrix[-test.id3,]),
  #        n.trees = 10000,
  #        interaction.depth = 1,
  #        shrinkage = 0.001,
  #        cv.folds = 5,
  #        n.cores = NULL,
  #        verbose = FALSE)
  
  gbm.fit2 <- gbm(
    formula = y ~ .,
    distribution = "gaussian",
    data =data.frame( xmatrix),
    n.trees = 5000,
    interaction.depth = 3,
    shrinkage = 0.1,
    cv.folds = 5,
    n.cores = NULL,
    verbose = FALSE)
  
  pred <- predict(gbm.fit.final2, n.trees = gbm.fit.final2$n.trees, data.frame(testmatrix))
  
  y.test=log(Housingcomplete[test.id5,82])
  library("caret")
  caret::RMSE(pred, testy)
  
  
  
  
  
  
 
 hyper_grid <- expand.grid(
        shrinkage = c(.01, .1, .3),
        interaction.depth = c(1, 3, 5),
        n.minobsinnode = c(5, 10, 15),
        bag.fraction = c(.65, .8, 1), 
        optimal_trees = 0,      
        min_RMSE = 0 )
 
 hyper_grid2 <- expand.grid(
   shrinkage = c(.01, .05, .1),
   interaction.depth = c(3, 5, 7),
   n.minobsinnode = c(5, 7, 10),
   bag.fraction = c(.65, .8, 1), 
   optimal_trees = 0,              
   min_RMSE = 0)
 # better grid
 
 
 # ames_train=Housingcomplete[-test.id,]
 random_index <- sample(1:nrow(train), nrow(train))
 random_ames_train <- train[random_index, ]
 
 for(i in 1:nrow(hyper_grid2)) {
   
   # reproducibility
   set.seed(123)
   
   # train model
   gbm.tune <- gbm(
     formula = y ~ .,
     distribution = "gaussian",
     data = random_ames_train,
     n.trees = 10000,
     interaction.depth = hyper_grid2$interaction.depth[i],
     shrinkage = hyper_grid2$shrinkage[i],
     n.minobsinnode = hyper_grid2$n.minobsinnode[i],
     bag.fraction = hyper_grid2$bag.fraction[i],
     train.fraction = .75,
     n.cores = NULL,
     verbose = FALSE
   )
   
   # add min training error and trees to grid
   hyper_grid2$optimal_trees[i] <- which.min(gbm.tune$valid.error)
   hyper_grid2$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
 }
 
 library(magrittr)
 hyper_grid %>% dplyr::arrange(min_RMSE) %>% head(10)
 
 # gbm.fit.final <- gbm(
 #   formula = log(Sale_Price) ~ .-PID-Street-Longitude-Latitude-Neighborhood,
 #   distribution = "gaussian",
 #   data = Housingcomplete[-test.id,],
 #   n.trees = 2771,
 #   interaction.depth = 5,
 #   shrinkage = 0.001,
 #   n.minobsinnode = 10,
 #   bag.fraction = .65, 
 #   train.fraction = 1,
 #   n.cores = NULL, 
 #   verbose = FALSE
 # )
 
 
 gbm.fit.final2 <- gbm(
        formula = y ~ .,
        distribution = "gaussian",
        data = data.frame(xmatrix),
        n.trees = 4000,
        interaction.depth = 5,
        shrinkage = 0.1,
        n.minobsinnode = 5,
        bag.fraction = .65, 
        train.fraction = 1,
        n.cores = NULL, 
        verbose = FALSE
    )

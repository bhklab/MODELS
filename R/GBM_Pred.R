############################# GBM_Pred is a function that models response classes using gradient boosting
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) TestFeat: Feature frame (rows as samples and columns as features) for testing

GBM_Pred <- function(TrainFeat, TrainObs, TestFeat){
  
  # Training <- data.frame(TrainFeat)
  TrainFeat$Observ <- TrainObs
  #################
  # nTree <- GBM_Optimize(TrainFeat,TrainObs)
  # print(paste("Best nTree is ", nTree, sep = "", collapse = ""))
  #################
  GBM_Model <- train(Observ~., method="gbm", data=TrainFeat, #tuneGrid=expand.grid(n.trees = nTree),
                    trControl = trainControl(method="repeatedcv", number=5,
                                             repeats = 5, search = "grid"))
  Testing <- data.frame(TestFeat)
  PredVal <- predict(GBM_Model, Testing)
  return(PredVal)
}



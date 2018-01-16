RF_Pred <- function(TrainFeat, TrainObs, TestFeat){
  
  # Training <- data.frame(TrainFeat)
  TrainFeat$Observ <- TrainObs
  #############
  # nTree <- 15
  # nTree <- RF_Optimize(TrainFeat,TrainObs)
  # print(paste("Best nTree is ", nTree, sep = "", collapse = ""))
  #############
  #nTree_grid <- seq(3, 20, 2)
  mtry <- sqrt(ncol(TrainFeat))
  
  trnCtrl <- trainControl(method = "repeatedCV",number = 5,repeats = 5)
  

  srchGrid <- expand.grid(.mtry=mtry) #.ntree = nTree_grid, 
  
  RF_Model <- caret::train(Observ~., method="rf", data=TrainFeat,  #ntree=nTree,
                tuneGrid = srchGrid,trControl = trnCtrl)
  
  Testing <- data.frame(TestFeat)
  PredVal <- predict(RF_Model, Testing)
  return(PredVal)
}



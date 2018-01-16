NNET_Pred <- function(TrainFeat, TrainObs, TestFeat){

  # Training <- data.frame(TrainFeat)
  TrainFeat$Observ <- TrainObs
  # Set up grid and cross validation method for train function
  Size_grid <- seq(5,10,1)
  
  trnCtrl <- trainControl(method = "repeatedCV",number = 5,repeats = 5)
  
  srchGrid <- expand.grid(.size = rep(Size_grid,5))
  
  Nnet_Model <- caret::train(Observ~., data=TrainFeat,method = "mlp", 
                        tuneGrid = srchGrid,trControl = trnCtrl)
  
  Testing <- data.frame(TestFeat)
  PredVal <- predict(Nnet_Model, Testing)
  return(PredVal)
}


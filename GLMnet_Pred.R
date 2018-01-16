GLMnet_Pred <- function(TrainFeat, TrainObs, TestFeat){

  # Training <- data.frame(TrainFeat)
  TrainFeat$Observ <- TrainObs
  # Set up grid and cross validation method for train function
  lambda_grid <- exp(seq(-5, 0, 1))
  alpha_grid <- seq(0, 1, 0.25)
  # print(TrainObs)
  
  trnCtrl <- trainControl(method = "repeatedCV",number = 5,repeats = 5)
  
  srchGrid <- expand.grid(.alpha = alpha_grid, .lambda = lambda_grid)
  
  GlMnet_Model <- caret::train(Observ~., data=TrainFeat, method = "glmnet", 
                        tuneGrid = srchGrid, trControl = trnCtrl)
  
  Testing <- data.frame(TestFeat)
  PredVal <- predict(GlMnet_Model, Testing)

  return(PredVal)
}


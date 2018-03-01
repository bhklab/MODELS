############################# NNET_Pred is a function that models response classes using  multilayer perceptron.
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) TestFeat: Feature frame (rows as samples and columns as features) for testing

NNET_Pred <- function(TrainFeat, TrainObs, TestFeat){

  # Training <- data.frame(TrainFeat)
  TrainFeat$Observ <- TrainObs
  # Set up grid and cross validation method for train function
  Size_grid <- seq(3,10,1)
  
  trnCtrl <- trainControl(method = "repeatedCV",number = 5,repeats = 5)
  
  srchGrid <- expand.grid(.size = rep(Size_grid,5))
  
  Nnet_Model <- caret::train(Observ~., data=TrainFeat,method = "mlp", 
                        tuneGrid = srchGrid,trControl = trnCtrl)
  
  Testing <- data.frame(TestFeat)
  PredVal <- predict(Nnet_Model, Testing)
  return(PredVal)
}


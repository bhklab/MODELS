############################# NB_Pred is a function that models response classes using naive bayes.
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) TestFeat: Feature frame (rows as samples and columns as features) for testing

NB_Pred <- function(TrainFeat, TrainObs, TestFeat){
  
  # Training <- data.frame(TrainFeat)
  TrainFeat$Observ <- TrainObs
  trnCtrl <- trainControl(method = "repeatedCV",number = 5,repeats = 5)
  
  srchGrid <- expand.grid(.usekernel = c(TRUE, FALSE), .fL = seq(0,1,0.1), .adjust = 1)
  
  NB_Model <- caret::train(Observ~., data=TrainFeat,method = "nb",
                             tuneGrid = srchGrid,trControl = trnCtrl)
  # print(NB_Model)
  Testing <- data.frame(TestFeat)
  PredVal <- predict(NB_Model, Testing)
  return(PredVal)
}


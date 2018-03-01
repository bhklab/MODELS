############################# GLMnet_Pred is a function that models response classes using regularized learnign (ridge, elasticnet, lasso). The regularization is optimized for lambda within training set.
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) TestFeat: Feature frame (rows as samples and columns as features) for testing

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


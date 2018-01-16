SVM_Pred <- function(TrainFeat, TrainObs, TestFeat){
  
  # Training <- data.frame(TrainFeat)
  TrainFeat$Observ <- TrainObs
  #############
  Method <- "svmLinear"#""svmRadial
  # Method <- SVM_Optimize(TrainFeat,TrainObs)
  # print(paste("Best method is ", Method, sep = "", collapse = ""))
  #############
  grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                       0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                             C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,1, 1.5, 2, 5))
  
  SVM_Model <- caret::train(Observ~., method=Method, data=TrainFeat, #tuneGrid = grid_radial,
                    tuneLength=10,trControl = trainControl(method="repeatedcv",
                                                           number=5, repeats = 5))
  Testing <- data.frame(TestFeat)
  PredVal <- predict(SVM_Model, Testing)
  return(PredVal)
}



############################# FeatSelect_RF is a function for feature selection based on random forrest
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) FeaturFrac: Fraction of features to be kept in output 

FeatSelect_RF <- function(TrainFeat, TrainObs,FeaturFrac){
  
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  results <- rfe(TrainFeat, TrainObs,
                 sizes=c(1:floor(FeaturFrac*ncol(TrainFeat))), rfeControl=control)
  ##########
  TargetFeatures <- sort(results$fit$importance[,"MeanDecreaseAccuracy"],
                         decreasing = T, index.return=T)[[2]][1:floor(FeaturFrac*ncol(TrainFeat))]
  
  return(na.omit(TargetFeatures))
}


############################# FeatSelect_mRMR is a function for feature selection based on mRMRe approach
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) FeaturFrac: Fraction of features to be kept in output 


FeatSelect_mRMR <- function(TrainFeat, TrainObs,FeaturFrac){
  threads <- get.thread.count()
  set.thread.count(threads)
  
  train_mRMR <- TrainFeat
  train_mRMR$Obs <- TrainObs
  FeatFrame_mRMR <- data.frame(train_mRMR)
  
  feature_data <- mRMR.data(data = FeatFrame_mRMR)#data.frame(TrainFeat))
  
  filter <- mRMR.classic("mRMRe.Filter", data = feature_data,
                         target_indices = which(colnames(train_mRMR) == "Obs"),
                         #solution_count = 10,
                         feature_count = floor(FeaturFrac*ncol(TrainFeat))) #floor(0.5*ncol(TrainFeat))
  
  return(unlist(solutions(filter)))
  
}
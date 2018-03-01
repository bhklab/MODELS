############################# FeatSelect_Var is a function for feature selection based on maximum variance (for numerical) and maximum entropy for categorical variables
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) FeaturFrac: Fraction of features to be kept in output 
############################# 4) FeatSelect_Num: logical variable to determine if the variables are numerical or not

FeatSelect_Var <- function(TrainFeat, TrainObs=NA,FeaturFrac,FeatSelect_Num=FALSE){
  
  if(FeatSelect_Num){
    MadVecC <- apply(TrainFeat, 2, mad)
    TargetFeatures <- sort(MadVecC, decreasing = T, index.return = T)[[2]][1:floor(FeaturFrac*ncol(TrainFeat))]
  }else{
    MadVecC <- apply(TrainFeat, 2, function(X){sum(-(table(X)/length(X))*log((table(X)/length(X))))})
    TargetFeatures <- sort(MadVecC, decreasing = T, index.return = T)[[2]][1:floor(FeaturFrac*ncol(TrainFeat))]
  }
  ###########
  return(TargetFeatures)
}
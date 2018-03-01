############################# FeatSelect_Wrap is a wrapper for all the feature selection approaches that user gives in input
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) FeaturFrac: Fraction of features to be kept in output 
############################# 4) FeatSelect_Num: logical variable to determine if the variables are numerical or not
############################# 5) FeatSelMethods: Names of feature selection appraoches (functions)

FeatSelect_Wrap <- function(TrainFeat, TrainObs, FeaturFrac,FeatSelect_Num, FeatSelMethods){
  
  TargetFeatures <- c(1:ncol(TrainFeat))
  
  for(ModelIter_Feat in 1:length(FeatSelMethods)){

    if(FeatSelMethods[ModelIter_Feat] == "Univar"){
      Uni_PredList <- Univar_Pred(TrainFeat, TrainObs,TrainFeat, FeaturFrac[ModelIter_Feat])
      SelectFeat <- Uni_PredList$GoodFeat
      TargetFeatures <- TargetFeatures[SelectFeat]
    }else{
      MatchedFun <- match.fun(FeatSelMethods[ModelIter_Feat])
      SelectFeat <- MatchedFun(TrainFeat, TrainObs, FeaturFrac[ModelIter_Feat],FeatSelect_Num)
      TargetFeatures <- TargetFeatures[SelectFeat]
    }

    TrainFeat <- TrainFeat[,SelectFeat]
  }
  ##########
  return(TargetFeatures)
}

FeatSelect <- function(FeatFrame,InputInd,TrainFeat, TestFeat, FeaturNum,FeatSelect_Num=FALSE,FeatSelect_Cat=FALSE){
  
  if(FeatSelect_Num){
    MadVecC <- apply(TrainFeat, 2, mad)
    TargetFeatures <- sort(MadVecC, decreasing = T, index.return = T)[[2]][1:FeaturNum]
    TrainFeat <- TrainFeat[,TargetFeatures]
    TestFeat <- FeatFrame[-InputInd,TargetFeatures]
  }else if(FeatSelect_Cat){
    MadVecC <- apply(TrainFeat, 2, function(X){sum(-(table(X)/length(X))*log((table(X)/length(X))))})
    TargetFeatures <- sort(MadVecC, decreasing = T, index.return = T)[[2]][1:FeaturNum]
    TrainFeat <- TrainFeat[,TargetFeatures]
    TestFeat <- FeatFrame[-InputInd,TargetFeatures]
  }else{
    TestFeat <- FeatFrame[-InputInd,]
  }
  ###########
  Featlist <- list(Training=TrainFeat, Testing=TestFeat)
  return(Featlist)
}
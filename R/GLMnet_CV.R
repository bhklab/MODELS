GLMnet_CV <- function(FeatMat, ObsVec, Family, Alpha, FeaturNum){
  
  FoldNum <- 10
  set.seed(32323)
  folds <- createFolds(y=ObsVec, k = FoldNum, list = TRUE, returnTrain = TRUE)
  
  PredVec <- c()
  ObservVec <- c()
  for(Folditer in 1:FoldNum){
    print(Folditer)
    InputInd <- folds[[Folditer]]
    ObservVec <- c(ObservVec, ObsVec[-InputInd])
    TrainFeat <- matrix(FeatMat[InputInd,], ncol = ncol(FeatMat))
    TrainObs <- ObsVec[InputInd]
    
    TestFeat <- matrix(FeatMat[-InputInd,], ncol = ncol(FeatMat))
    PredVec <- c(PredVec, GLMnet_Pred(TrainFeat, TrainObs,
                                      TestFeat, Family, Alpha, FeaturNum))
  }
  ######
  return(cbind(PredVec, ObservVec))
}

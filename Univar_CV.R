Univar_CV <- function(FeatMat, ObsVec, FeaturNum){
  
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
    PredVec <- c(PredVec, Univar_Pred(TrainFeat, TrainObs,TestFeat, FeaturNum))
  }
  ######
  return(cbind(PredVec, ObservVec))
}

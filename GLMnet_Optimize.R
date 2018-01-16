GlMnet_Optimize <- function(FeatMat,ObsVec){
  
  Alpha_Vec  <- seq(0,1,0.25)
  MCCmedian_Vec <- c()
  for(alpha in Alpha_Vec){
    MCC_Vec <- c()
    for(BSiter in 1:10){
      FoldNum <- 3
      folds <- createFolds(y=ObsVec, k = FoldNum, list = TRUE, returnTrain = TRUE)
      
      Folditer <- 1
      InputInd <- folds[[Folditer]]
      ObservVec <- ObsVec[-InputInd]
      TrainFeat <- matrix(FeatMat[InputInd,], ncol = ncol(FeatMat))
      TrainObs <- ObsVec[InputInd]
      
      TestFeat <- matrix(FeatMat[-InputInd,], ncol = ncol(FeatMat))
      GlMnet_Model <- cv.glmnet(x = TrainFeat, y = TrainObs,
                                alpha=alpha, nfolds = 5,family = "binomial")
      PredVec <- predict(GlMnet_Model, TestFeat, type="class",s="lambda.min")
      
      if(length(unique(PredVec)) < length(unique(ObservVec))){
        MCC_Vec <- c(MCC_Vec, NA)
      }else{
        MCC_Vec <- c(MCC_Vec, mcc(factor(PredVec), factor(ObservVec), nperm = 2)$estimate)
      }
    }
    MCCmedian_Vec <- c(MCCmedian_Vec, median(na.omit(MCC_Vec)))
  }
  
  print(MCCmedian_Vec)
  Opt_alpha <- Alpha_Vec[which(MCCmedian_Vec == max(na.omit(MCCmedian_Vec)))[
    sample.int(length(which(MCCmedian_Vec == max(na.omit(MCCmedian_Vec)))),1)]]
  #########
  return(Opt_alpha)
}


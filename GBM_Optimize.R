GBM_Optimize <- function(FeatFrame,ObsVec){
  
  nTree <- seq(2,20,3)
  MCCmedian_Vec <- c()
  for(TreeNum in nTree){
    MCC_Vec <- c()
    for(BSiter in 1:10){
      FoldNum <- 3
      folds <- createFolds(y=ObsVec, k = FoldNum, list = TRUE, returnTrain = TRUE)
      
      Folditer <- 1
      InputInd <- folds[[Folditer]]
      ObservVec <- ObsVec[-InputInd]
      # Training <- data.frame(matrix(FeatMat[InputInd,], ncol = ncol(FeatMat)))
      Training <- FeatFrame[InputInd,]
      
      Training$Observ <- ObsVec[InputInd]
      # Testing <- data.frame(matrix(FeatMat[-InputInd,], ncol = ncol(FeatMat)))
      Testing <- FeatFrame[-InputInd,]
      
      
      GBM_Model <- train(Observ~., method="gbm", data=Training, tuneGrid=expand.grid(n.trees = nTree),
                         trControl = trainControl(method="repeatedcv", number=5,
                                                  repeats = 5, search = "grid"))
      PredVec <- predict(GBM_Model, Testing)
      
      if(length(unique(PredVec)) < length(unique(ObservVec))){
        MCC_Vec <- c(MCC_Vec, NA)
      }else{
        MCC_Vec <- c(MCC_Vec, mcc(factor(PredVec), factor(ObservVec), nperm = 2)$estimate)
      }
    }
    MCCmedian_Vec <- c(MCCmedian_Vec, median(na.omit(MCC_Vec)))
  }
  
  print(Opt_nTree)
  Opt_nTree <- nTree[which(MCCmedian_Vec == max(na.omit(MCCmedian_Vec)))[
    sample.int(length(which(MCCmedian_Vec == max(na.omit(MCCmedian_Vec)))),1)]]
  #########
  return(Opt_nTree)
}


Univar_Optimize <- function(FeatFrame,ObsVec){
  # colnames(FeatFrame) <- NULL
  # rownames(FeatFrame) <- NULL
  MCCmedian_Vec <- c()
  for(FeatIter in 1:ncol(FeatFrame)){
    Feat_Filt <- FeatFrame[,FeatIter]
    
    MCC_Vec <- c()
    for(BSiter in 1:10){
      FoldNum <- 10
      folds <- createFolds(y=ObsVec, k = FoldNum, list = TRUE, returnTrain = TRUE)
      
      Folditer <- 1
      InputInd <- folds[[Folditer]]
      
      Training <- data.frame(Feat_Filt[InputInd])
      names(Training) <- "FeaturIter"
      Training$Observ <- ObsVec[InputInd]
      
      Testing <- data.frame(Feat_Filt[-InputInd])
      names(Testing) <- "FeaturIter"
      ObservVec <- ObsVec[-InputInd]
      # print(length(unique(Training$FeaturIter)))
      if(length(unique(Training$FeaturIter)) > 1){
        Univar_Model <- glm(Observ~., data = Training, family = binomial) 
        PredVec <- as.numeric(ceiling(2*predict(Univar_Model, Testing,type="response")))

        # print(unique(PredVec))
        # if(length(which(is.na(PredVec))) > 0){
        #   print(predict(Univar_Model, Testing,type="response"))
        # }
        # print(unique(ObservVec))
        # print("OK")
        # print(length(PredVec))
        # print(length(ObservVec))        
        if(length(unique(PredVec)) < length(unique(ObservVec))){
          MCC_Vec <- c(MCC_Vec, NA)
        }else{
          MCC_Vec <- c(MCC_Vec, mcc(factor(PredVec, ordered = T), factor(ObservVec, ordered = T),
                                    nperm = 2)$estimate)
        }
        
      }else{
        MCC_Vec <- c(MCC_Vec, NA)
      }
    }
    MCCmedian_Vec <- c(MCCmedian_Vec, median(na.omit(MCC_Vec)))
  }
  
  GoodFeat_Cut <- NA
  CutIter <- 0.2
  while(is.na(GoodFeat_Cut)){
    
    if(length(which(MCCmedian_Vec > CutIter) > 20)){
      GoodFeat_Cut <- CutIter
    }
    CutIter <- CutIter-0.05
  }
  # GoodFeat_Cut <- ifelse(length(which(MCCmedian_Vec > 0.2)) > 20, 0.2,
  #                    ifelse(length(which(MCCmedian_Vec > 0.1)) > 20, 0.1,0))
  GoodFeat <- which(MCCmedian_Vec > GoodFeat_Cut)

  print(quantile(na.omit(MCCmedian_Vec)))
  BestFeat <- which(MCCmedian_Vec == max(na.omit(MCCmedian_Vec)))[sample.int(
    length(which(MCCmedian_Vec == max(na.omit(MCCmedian_Vec)))),1)]
  
  FeatList <- list(GoodFeat=GoodFeat, BestFeat=BestFeat)
  ############
  return(FeatList)
}




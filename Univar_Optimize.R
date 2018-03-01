Univar_Optimize <- function(FeatFrame,ObsVec, FeatFrac){

  MCCmedian_Vec <- c()
  for(FeatIter in 1:ncol(FeatFrame)){
    Feat_Filt <- FeatFrame[,FeatIter]
    
    MCC_Vec <- c()
    for(BSiter in 1:10){
      FoldNum <- 5
      folds <- createFolds(y=ObsVec, k = FoldNum, list = TRUE, returnTrain = TRUE)
      
      Folditer <- 1
      InputInd <- folds[[Folditer]]
      
      Training <- data.frame(Feat_Filt[InputInd])
      names(Training) <- "FeaturIter"
      Training$Observ <- ObsVec[InputInd]
      
      Testing <- data.frame(Feat_Filt[-InputInd])
      names(Testing) <- "FeaturIter"

      
      ObservVec <- ObsVec[-InputInd]


      if(length(unique(Training$FeaturIter)) > 1){
        Univar_Model <- glm(Observ~., data = Training, family = binomial) 
        
        PredVec <- as.numeric(ceiling(2*predict(Univar_Model, Testing,type="response")))
        
        if(length(unique(PredVec)) < length(unique(ObservVec)) | length(unique(PredVec)) == 1 |
           length(unique(ObservVec)) == 1){
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
  #############
  GoodFeat_Cut <- NA
  CutIter <- 0.5
  while(is.na(GoodFeat_Cut)){
    if(length(which(MCCmedian_Vec > CutIter)) > floor(FeatFrac*length(na.omit(MCCmedian_Vec))) | CutIter <= 0){
      GoodFeat_Cut <- CutIter
    }
    CutIter <- CutIter-0.05
  }
  #############
  GoodFeat <- which(MCCmedian_Vec > GoodFeat_Cut)
  
  BestFeat <- which(MCCmedian_Vec == max(na.omit(MCCmedian_Vec)))[sample.int(
    length(which(MCCmedian_Vec == max(na.omit(MCCmedian_Vec)))),1)]
  
  FeatList <- list(GoodFeat=GoodFeat, BestFeat=BestFeat)
  ############
  return(FeatList)
}




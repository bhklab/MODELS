Univar_Pred <- function(TrainFeat, TrainObs, TestFeat){
  
  #############
  FeatList <- Univar_Optimize(TrainFeat,TrainObs)
  #############
  # TrainFrame <- data.frame(TrainFeat[,c(FeatList$BestFeat)])
  TrainFrame <- data.frame(TrainFeat[,FeatList$BestFeat])
  names(TrainFrame) <- "BestFeat"
  TrainFrame$Obs <- TrainObs
  # print(dim(TrainFrame))
  
  # TestFrame <- data.frame(TestFeat[,c(FeatList$BestFeat)])
  TestFrame <- data.frame(TestFeat[,FeatList$BestFeat])
  names(TestFrame) <- "BestFeat"
  
  
  Glm_Model <- glm(Obs~., data = TrainFrame, family = binomial)
  PredVal <- as.numeric(ceiling(2*predict(Glm_Model, TestFrame,
                                        type="response")))
  PredFeatList <- list(PredVal=PredVal, GoodFeat=FeatList$GoodFeat)
  return(PredFeatList)
}

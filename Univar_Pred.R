############################# SVM_Pred is a function that models response classes using the best univariate model.
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) TestFeat: Feature frame (rows as samples and columns as features) for testing
############################# 4) FeatFrac: Maximum fraction of features to be filtered in feature selection process using univariate models

Univar_Pred <- function(TrainFeat, TrainObs, TestFeat, FeatFrac=0.5){
  
  #############
  FeatList <- Univar_Optimize(TrainFeat,TrainObs, FeatFrac)
  #############
  TrainFrame <- data.frame(TrainFeat[,FeatList$BestFeat])
  names(TrainFrame) <- "BestFeat"
  TrainFrame$Obs <- TrainObs
  ############
  TestFrame <- data.frame(TestFeat[,FeatList$BestFeat])
  names(TestFrame) <- "BestFeat"
  
  
  Glm_Model <- glm(Obs~., data = TrainFrame, family = binomial)
  ##########
  TrainFrame <- data.frame(TrainFeat[,FeatList$BestFeat])
  names(TrainFrame) <- "BestFeat"
  
  Pred_Train <- predict(Glm_Model, TrainFrame, type="response")
  PredVal <- as.numeric(predict(Glm_Model, TrainFrame,type="response")) #(1/ProbCut)
  
  ProbCut <- ProbCut_Optimize(PredVal, TrainObs)

  print(ProbCut)
  PredVal <- as.numeric(predict(Glm_Model, TestFrame,type="response")) #(1/ProbCut)
  PredVal <- unlist(lapply(PredVal, function(X){ifelse(X > ProbCut, 2, 1)}))
  

  PredVal[which(PredVal >= max(as.numeric(TrainObs)))] <- max(as.numeric(TrainObs))
  PredVal[which(PredVal <= min(as.numeric(TrainObs)))] <- min(as.numeric(TrainObs))

  
  PredFeatList <- list(PredVal=PredVal, GoodFeat=FeatList$GoodFeat)
  return(PredFeatList)
}

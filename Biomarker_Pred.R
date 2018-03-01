############################# Biomarker_Pred is a function that models response classes as a univariate or multivariate logistic regression using user-defined biomarkers
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) TestFeat: Feature frame (rows as samples and columns as features) for testing
############################# 4) Biomarkers: User-defined biomarkers (for example a vector of gene names if biomarkers are exression of those genes)

Biomarker_Pred <- function(TrainFeat, TrainObs, TestFeat, Biomarkers){

  TargetFeat <- which(gsub("X", "", colnames(TrainFeat)) %in% Biomarkers)
  TrainFrame <- data.frame(TrainFeat[,TargetFeat])
  colnames(TrainFrame) <- paste("Biomarker", seq(1,length(TargetFeat)), "_")
  TrainFrame$Obs <- TrainObs

  TestFrame <- data.frame(TestFeat[,TargetFeat])
  colnames(TestFrame) <- paste("Biomarker", seq(1,length(TargetFeat)), "_")
  Glm_Model <- glm(Obs~., data = TrainFrame, family = binomial)
  ################
  TrainFrame <- data.frame(TrainFeat[,TargetFeat])
  colnames(TrainFrame) <- paste("Biomarker", seq(1,length(TargetFeat)), "_")
  ###############
  Pred_Train <- predict(Glm_Model, TrainFrame, type="response")
  ProbCut <- max(c(min(Pred_Train[which(TrainObs == 2)]), 
                      max(Pred_Train[which(TrainObs == 1)])))
  print(ProbCut)
  PredVal <- as.numeric(predict(Glm_Model, TestFrame,type="response")) #(1/ProbCut)
  PredVal <- unlist(lapply(PredVal, function(X){ifelse(X > ProbCut, 2, 1)}))

  PredVal[which(PredVal >= max(as.numeric(TrainObs)))] <- max(as.numeric(TrainObs))
  PredVal[which(PredVal <= min(as.numeric(TrainObs)))] <- min(as.numeric(TrainObs))

  return(PredVal)
}

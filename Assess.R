############################# Assess is a post-processing function for assessment of predictions
############################# Input variables of this function are as follows:
############################# 1) Predictions: List of predictions using different predictive models for one or more sampling
############################# 2) Observations: List of observed classes for one or more sampling  

Assess <- function(Predictions, Observations){
  
  MCC_Mat <- c()
  AUCroc_Mat <- c()
  AUCpr_Mat <- c()
  F1_Mat <- c()
  for(Samiter in 1:length(Predictions)){
    PredMat <- Predictions[[Samiter]]
    ObservVec <- Observations[[Samiter]]
    MCC_Vec <- c()
    AUCroc_Vec <- c()
    AUCpr_Vec <- c()
    F1_Vec <- c()
    for(MethodIter in 1:ncol(PredMat)){
      PredVec <- PredMat[,MethodIter]
      if(length(unique(PredVec)) < length(unique(ObservVec))){
        MCC_Vec <- c(MCC_Vec, NA)
        AUCroc_Vec <- c(AUCroc_Vec, NA)
        AUCpr_Vec <- c(AUCpr_Vec, NA)
        F1_Vec <- c(F1_Vec, NA)
      }else{

        MCC_Vec <- c(MCC_Vec, mcc(factor(PredVec), factor(ObservVec), nperm = 2)$estimate)
        AUCroc_Vec <- c(AUCroc_Vec, auc(roc(as.numeric(ObservVec), as.numeric(PredVec))))
        AUCpr_Vec <- c(AUCpr_Vec, pr.curve(as.numeric(ObservVec), as.numeric(PredVec), curve = T)$auc.integral)
        F1_Vec <- c(F1_Vec, F1_Score(as.numeric(ObservVec), as.numeric(PredVec), positive = NULL))
      }
    }
    
    MCC_Mat <- rbind(MCC_Mat, MCC_Vec)
    AUCroc_Mat <- rbind(AUCroc_Mat, AUCroc_Vec)
    AUCpr_Mat <- rbind(AUCpr_Mat, AUCpr_Vec)
    F1_Mat <- rbind(F1_Mat, F1_Vec)
  }
  
  colnames(MCC_Mat) <- colnames(PredMat)
  colnames(AUCroc_Mat) <- colnames(PredMat)
  colnames(AUCpr_Mat) <- colnames(PredMat)
  colnames(F1_Mat) <- colnames(PredMat)
  
  rownames(MCC_Mat) <- names(Predictions)
  rownames(AUCroc_Mat) <- names(Predictions)
  rownames(AUCpr_Mat) <- names(Predictions)
  rownames(F1_Mat) <- names(Predictions)
  ############
  AssessList <- list(MCC=MCC_Mat, AUC_ROC= AUCroc_Mat, AUC_PR = AUCpr_Mat, F1 = F1_Mat)
  
  return(AssessList)
}



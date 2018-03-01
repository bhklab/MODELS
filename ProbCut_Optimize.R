############################# ProbCut_Optimize is a function for identifying optimum cutoff for posterior probability of the predictions in training set to be used for classifying the samples in test set
############################# Input variables of this function are as follows:
############################# 1) PredVal: Posterior probability of the predictions for the observed classes in the training set
############################# 2) TrainObs: Observed classes in the training set

ProbCut_Optimize <- function(PredVal, TrainObs){
  
  MCC_Vec <- c()
  for(ProbCut in seq(0.9,0.1,-0.01)){
    PredVal_Class <- unlist(lapply(PredVal, function(X){ifelse(X > ProbCut, 2, 1)}))
    if(length(unique(PredVal_Class)) < length(unique(TrainObs)) | length(unique(PredVal_Class)) == 1 |
       length(unique(TrainObs)) == 1){
      MCC_Vec <- c(MCC_Vec, NA)
    }else{
      MCC_Vec <- c(MCC_Vec, mcc(factor(PredVal_Class, ordered = T), factor(TrainObs, ordered = T),
                                nperm = 2)$estimate)
    }
  }
  
  # ProbCut <- seq(0.9,0.1,-0.01)[which(MCC_Vec == max(na.omit(MCC_Vec)))[
  #   sample.int(length(which(MCC_Vec == max(na.omit(MCC_Vec)))),1)]]
  ProbCut <- max(seq(0.9,0.1,-0.01)[which(MCC_Vec == max(na.omit(MCC_Vec)))])
  
  if(length(ProbCut) == 0){
    ProbCut <- 0.5
  }
  
  return(ProbCut)
  
}
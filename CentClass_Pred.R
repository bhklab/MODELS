############################# CentClass_Pred is a function that models response classes using centroid classification approach consideting pearson correlation as measure of similarity between samples
############################# Input variables of this function are as follows:
############################# 1) TrainFeat: Feature frame (rows as samples and columns as features) for training set
############################# 2) TrainObs: Observed classess for training
############################# 3) TestFeat: Feature frame (rows as samples and columns as features) for testing

CentClass_Pred <- function(TrainFeat, TrainObs, TestFeat){
  
  ClassName <- unique(TrainObs)
  ############
  ClassAveMat <- c()
  for(ClassIter in 1:length(ClassName)){
    ClassMatchInd <- which(TrainObs == ClassName[ClassIter])
    ClassAveMat <- rbind(ClassAveMat, apply(TrainFeat[ClassMatchInd,], 2, median))
  }
  ############
  PredVal <- c()
  for(TestSamIter in 1:nrow(TestFeat)){
    SimCor <- unlist(lapply(1:nrow(ClassAveMat), function(Iter){cor.test(as.numeric(ClassAveMat[Iter,]),
                                                                         as.numeric(TestFeat[TestSamIter,]),
                                                                         method = "s")$estimate}))
    PredVal <- c(PredVal, ClassName[which(SimCor == max(SimCor))[
      sample.int(length(which(SimCor == max(SimCor))),1)]])
  }
  ############
  return(PredVal)
}


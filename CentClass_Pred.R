CentClass_Pred <- function(TrainFeat, TrainObs, TestFeat){
  
  ClassName <- unique(TrainObs)
  
  ClassAveMat <- c()
  for(ClassIter in 1:length(ClassName)){
    ClassMatchInd <- which(TrainObs == ClassName[ClassIter])
    ClassAveMat <- rbind(ClassAveMat, apply(TrainFeat[ClassMatchInd,], 2, median))
  }
  
  PredVal <- c()
  for(TestSamIter in 1:nrow(TestFeat)){
    SimCor <- unlist(lapply(1:nrow(ClassAveMat), function(Iter){cor.test(as.numeric(ClassAveMat[Iter,]),
                                                                         as.numeric(TestFeat[TestSamIter,]), method = "s")$estimate}))
    PredVal <- c(PredVal, ClassName[which(SimCor == max(SimCor))[
      sample.int(length(which(SimCor == max(SimCor))),1)]])
  }
  
  return(PredVal)
}


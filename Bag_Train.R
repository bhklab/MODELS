Bag_Train <- function(TrainObs){
  
  Classes <- unique(TrainObs)
  MinNum <- min(as.numeric(table(TrainObs)))
  RandSam <- c()
  for(ClassIter in 1:length(Classes)){
    RandSam <- c(RandSam, sample(which(TrainObs == Classes[ClassIter]),
                                     MinNum, replace = FALSE))
  }
  return(unique(RandSam))
}
BackgroundAdd <- function(TargetFeatFrame, ObservationVec, MinRange,MaxRnage, AddClass, AddNum){
  
  TargetFeatFrame <- ProcessedData$FeatFrame
  FeatFrame_Aux <- TargetFeatFrame
  for(Iter in 1:AddNum){
    FeatFrame_Aux <- rbind(FeatFrame_Aux, runif(ncol(TargetFeatFrame), MinRange,MaxRnage))
    ObservationVec <- c(ObservationVec, AddClass)
  }
  TargetFeatFrame <- FeatFrame_Aux

  ObservationVec <- factor(ObservationVec, ordered = T)
  
  BackAdded <- list(Feat=TargetFeatFrame, Observation=ObservationVec)
  return(BackAdded)
}

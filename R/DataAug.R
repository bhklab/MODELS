############################# DataAug is a function for data augmentation for imbalanced observed classes (in trainign set)
############################# Input variables of this function are as follows:
############################# 1) FeatFrame: Feature frame (rows as samples and columns as features) 
############################# 2) ObsVec: Observed classess
############################# 3) Aug_VarCutoff: Maximum variance to be used for data augmentation

DataAug <- function(FeatFrame, ObsVec, Aug_VarCutoff=0){
  
  ObsTable <- table(ObsVec)
  AugNum <- (max(as.numeric(ObsTable))-min(as.numeric(ObsTable)))
  AugClass <- names(ObsTable)[which(as.numeric(ObsTable) == min(as.numeric(ObsTable)))]
  ClassInd <- which(ObsVec == AugClass)
  ##############
  FeatMat <- as.matrix(FeatFrame)
  
  MadVec <- apply(FeatMat[ClassInd,], 2,mad)
  AugMat <- FeatMat
  if(AugNum >= 1){
    if(AugClass == 1){
      for(AugIter in 1:AugNum){
        TargetInd <- sample(ClassInd, 1)
        
        AugVec <- unlist(lapply(1:length(MadVec), function(Iter){
          runif(1,0, 0.1)}))
        AugMat <- rbind(AugMat, AugVec)
      }
    }else{
      for(AugIter in 1:AugNum){
        TargetInd <- sample(ClassInd, 1)
        
        AugVec <- unlist(lapply(1:length(MadVec), function(Iter){
          rnorm(1,FeatMat[TargetInd,Iter], Aug_VarCutoff*MadVec[Iter])}))
        
        AugMat <- rbind(AugMat, AugVec)
      }
    }
    
  }
  AugFrame <- as.data.frame(AugMat)
  names(AugFrame) <- names(FeatFrame)
  ObsVec <- as.numeric(c(ObsVec, rep(AugClass, AugNum)))
  
  OutList <- list(Feat=AugFrame,Obs=factor(ObsVec, ordered = T))
  return(OutList)
}


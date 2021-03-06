############################# FeatTrans_Path is a feature transformation function to discretize the input continuous variables
############################# Input variables of this function are as follows:
############################# 1) FeatFrame: Feature frame (rows as samples and columns as features) 
############################# 2) TargetFeat: Features to be discretized and returned as output
############################# 3) ClustNum: Number of classes for the discretized version of the variables in output 

FeatTransform_Bin <- function(FeatureFrame, TargetFeat, ClustNum=2){
  
  for(FeatIter in TargetFeat){
    print(FeatIter)
    FeatVec <- as.numeric(FeatureFrame[,FeatIter])
    ###########
    SimMat <- c()
    for(SamIter in 1:length(FeatVec)){
      SimMat <- rbind(SimMat, (1-abs(FeatVec[SamIter]-FeatVec
                                     )/max(abs(FeatVec[SamIter]-FeatVec))))
    }
    
    # Clusters <- apcluster(SimMat,q=0.01)@clusters
    # TformedFeat <- Mclust(SimMat, G=1:2)$classification
    TformedFeat <- kmeans(SimMat, 2)$cluster
    ##########
    # TformedFeat <- rep(1, length(FeatVec))
    # for(ClustIter in 1:length(Clusters)){
    #   ClustInd <- Clusters[[ClustIter]]
    #   TformedFeat[ClustInd] <- ClustIter
    # }
    
    # RemInd <- which(table(TformedFeat)/length(TformedFeat) < 0.1)
    # for(Iter in RemInd){
    #   if(Iter > 1){
    #     TformedFeat[which(TformedFeat == Iter)] <- (Iter-1)
    #   }else if(Iter == 1){
    #     TformedFeat[which(TformedFeat == Iter)] <- (Iter+1)
    #   }
    # }
    FeatureFrame[,FeatIter] <- factor(TformedFeat, ordered = T)
  }
  
  FeatureFrame_Bin <- data.frame(FeatureFrame[,TargetFeat])
  ###########
  return(FeatureFrame_Bin)
}


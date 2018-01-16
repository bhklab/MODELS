PreProcess <- function(PSet, Tissue, Drug, MolecularProfile, Measure, HighCutoff, LowCutoff){
  
  TargetPset <- downloadPSet(PSet)
  ExpProfile <- exprs(summarizeMolecularProfiles(TargetPset,  mDataType = MolecularProfile, verbose=TRUE))
  rownames(ExpProfile) <- as.numeric(fData(TargetPset@molecularProfiles[[MolecularProfile]])[,"EntrezGeneId"])
  ############# lung, haematopoietic_and_lymphoid_tissue, skin, central_nervous_system, large_intestine, breast
  MatchedCellId <- which(TargetPset@cell$tissueid %in% Tissue)
  TargetCells <- TargetPset@cell$cellid[MatchedCellId]
  #############
  TissuVec <- factor(TargetPset@cell$tissueid[MatchedCellId])
  #############
  ObsMat <- c()
  for(MeasureIter in 1:length(Measure)){
    SensProfile <- summarizeSensitivityProfiles(TargetPset, sensitivity.measure = Measure[MeasureIter])
    
    ContVec <- SensProfile[which(tolower(as.character(rownames(SensProfile))) == tolower(Drug)), 
                           which(toupper(colnames(SensProfile)) %in%
                                   intersect(toupper(colnames(SensProfile)), TargetCells))]
    
    if(Measure[MeasureIter] == "ic50_recomputed"){
      ClassVec <- ifelse(ContVec < LowCutoff[MeasureIter], 1, ifelse(ContVec > HighCutoff[MeasureIter], 2,NA))
    }else{
      ClassVec <- ifelse(ContVec > HighCutoff[MeasureIter], 1, ifelse(ContVec < LowCutoff[MeasureIter], 2,NA))
    }
    ObsMat <- rbind(ObsMat, ClassVec)
  }
  
  SumVec <- apply(ObsMat, 2, function(X){length(unique(X))})
  TargetInd <- which(apply(ObsMat, 2, function(X){length(unique(X))}) == 1 & 
                       apply(ObsMat, 2, function(X){length(which(is.na(X)))}) == 0)
  Classes <- unlist(apply(ObsMat, 2, unique)[TargetInd])
  ##############
  # TargetInd <- which(!is.na(ObsVec) & ObsVec != Inf)
  # ObsVec <- as.numeric(ObsVec[TargetInd])
  
  FeatureMat <- t(as.matrix(ExpProfile[,TargetInd]))
  TissuVec <- TissuVec[TargetInd]
  
  rownames(FeatureMat) <- NULL
  colnames(FeatureMat) <- rownames(ExpProfile)
  
  FeatureFrame <- data.frame(FeatureMat)
  #############
  # Cutoff_High <- HighCutoff
  # Cutoff_Low <- LowCutoff
  # RemInd <- which(ObsVec >= Cutoff_Low & ObsVec <= Cutoff_High)
  # 
  # if(length(RemInd) > 0){
  #   ObsVec <- ObsVec[-RemInd]
  #   FeatureFrame <- FeatureFrame[-RemInd, ]
  #   TissuVec <- TissuVec[-RemInd]
  # }
  
  # Classes <- ifelse(ObsVec > Cutoff_High, 1, 2)
  ############
  ProcessedList <- list(FeatFrame=FeatureFrame, Observations = Classes, Tissues=TissuVec)
  
  return(ProcessedList)
}


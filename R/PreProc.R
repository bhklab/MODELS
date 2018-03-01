PreProc <- function(PSetName, MolecularProfile, ResponseMeasure, MeasureCutOff,
                    Tissues, Drug, Methods){
  
  PSet <- downloadPSet(PSetName)
  SensProfile <- summarizeSensitivityProfiles(PSet, sensitivity.measure = "auc_recomputed")
  ExpProfile <- exprs(summarizeMolecularProfiles(PSet,  mDataType = "rna", verbose=TRUE))
  
  
  TargetCells <- PSet@cell$cellid[which(GDSC1000@cell$tissueid %in% Tissues)]
  
  ObsVec <- SensProfile[which(tolower(as.character(rownames(SensProfile))) == tolower(Drug)),
                        which(toupper(colnames(SensProfile)) %in%
                                intersect(toupper(colnames(SensProfile)), TargetCells))]
  ################
  TargetInd <- which(!is.na(ObsVec) & ObsVec != Inf)
  ObsVec <- as.numeric(ObsVec[TargetInd])
  
  ExpMat <- normalize.quantiles(as.matrix(ExpProfile[,TargetInd]))
  colnames(ExpMat) <- NULL
  rownames(ExpMat) <- rownames(ExpProfile)
  #################
  Cutoff <- MeasureCutOff
  
  Classes <- ifelse(ObsVec > Cutoff, "High", "Low")
  table(Classes)
  
}
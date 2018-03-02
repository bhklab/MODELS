############################# PreProcess is a function which extract the feature and observation information data from PharmacoGx package for the drug and tissues onf interest and return them in format ready for direct use in the modeling functions.
############################# Input variables of this function are as follows:
############################# 1) PSet: Name of PSet in PharmacoGx package to extract the pharmacogenomics data from
############################# 2) Tissue: Vector of tissue names. All cell lines in these tissues which have been tested for the target drug and their responses are in the determined range will be included in the output
############################# 3) Drug: Name of drug that its responses in cell lines of determined tissues will be modeled
############################# 4) MolecularProfile: The type of molecular profile (Ex. "rna" for gene expression)
############################# 5) Measure: Measure of sensitivity (Ex. "auc_recomputed" for recomputed area under the curve in PharmacoGx package)
############################# 6) HighCutoff: Cutoff for minimum value of the sensitivity of each cell line for which the cell line is considered as sensitive
############################# 7) HighCutoff: Cutoff for maximum value of the sensitivity of each cell line for which the cell line is considered as in-sensitive

PreProcess <- function(PSet, Tissue, Drug, MolecularProfile, Measure, HighCutoff, LowCutoff){

  TargetPset <- downloadPSet(PSet)
  ExpProfile <- exprs(summarizeMolecularProfiles(TargetPset,  mDataType=MolecularProfile, verbose=TRUE, cell.lines=TargetPset@cell$cellid[which(TargetPset@cell$tissueid %in% Tissue)]))
  col_with_NA <- unique(which(is.na(ExpProfile), arr.ind=T)[,2])
  ExpProfile <- ExpProfile[, -col_with_NA]
  ############# lung, haematopoietic_and_lymphoid_tissue, skin, central_nervous_system, large_intestine, breast
  TargetCells <- colnames(ExpProfile)
  #############
  TissuVec <- factor(which(TargetPset@cell$tissueid %in% Tissue)[-col_with_NA])
  #############
  ObsMat <- c()
  for(MeasureIter in 1:length(Measure)){
    SensProfile <- summarizeSensitivityProfiles(TargetPset, sensitivity.measure=Measure[MeasureIter])

    ContVec <- SensProfile[which(tolower(as.character(rownames(SensProfile))) == tolower(Drug)),
                           which(toupper(colnames(SensProfile)) %in%
                                   intersect(toupper(colnames(SensProfile)), TargetCells))]

    if(Measure[MeasureIter] == "ic50_recomputed"){
      ClassVec <- ifelse(ContVec < LowCutoff[MeasureIter], 2, ifelse(ContVec > HighCutoff[MeasureIter], 1, NA))
    }else{
      ClassVec <- ifelse(ContVec > HighCutoff[MeasureIter], 2, ifelse(ContVec < LowCutoff[MeasureIter], 1, NA))
    }

    ObsMat <- rbind(ObsMat, ClassVec)
  }

  SumVec <- apply(ObsMat, 2, function(X){length(unique(X))})
  TargetInd <- which(apply(ObsMat, 2, function(X){length(unique(X))}) == 1 &
                       apply(ObsMat, 2, function(X){length(which(is.na(X)))}) == 0)
  Classes <- unlist(apply(ObsMat, 2, unique)[TargetInd])
  ##############
  FeatureMat <- t(as.matrix(ExpProfile[,TargetInd]))
  TissuVec <- TissuVec[TargetInd]

  rownames(FeatureMat) <- NULL
  colnames(FeatureMat) <- rownames(ExpProfile)

  FeatureFrame <- data.frame(FeatureMat)
  ###########
  ProcessedList <- list(FeatFrame=FeatureFrame, Observations = Classes, Tissues=TissuVec)

  return(ProcessedList)
}


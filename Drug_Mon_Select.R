rm(list = ls())
library(PharmacoGx)
library(Biobase)
library(preprocessCore)
#################
GDSC1000 <- downloadPSet("GDSC1000")
SensProfile <- summarizeSensitivityProfiles(GDSC1000, sensitivity.measure = "auc_recomputed")
ExpProfile <- exprs(summarizeMolecularProfiles(GDSC1000,  mDataType = "rna", verbose=TRUE))


Cells_Lung <- GDSC1000@cell$cellid[which(GDSC1000@cell$tissueid %in% "lung")]
Cells_Hemato <- GDSC1000@cell$cellid[which(GDSC1000@cell$tissueid
                                          == "haematopoietic_and_lymphoid_tissue")]
Cells_Skin <- GDSC1000@cell$cellid[which(GDSC1000@cell$tissueid== "skin")]
Cells_CNS <- GDSC1000@cell$cellid[which(GDSC1000@cell$tissueid== "central_nervous_system")]
Cells_Intestine <- GDSC1000@cell$cellid[which(GDSC1000@cell$tissueid== "large_intestine")]
Cells_Breast <- GDSC1000@cell$cellid[which(GDSC1000@cell$tissueid== "breast")]

CellList <- list(Lung = Cells_Lung, Hemato = Cells_Hemato,
                 Skin = Cells_Skin, CNS = Cells_CNS,
                 Intestine = Cells_Intestine, Breast = Cells_Breast)


SamNum_Mat <- c()
Cutoff <- 0.2
ClassFrac_Mat <- c()
for(TissueIter in 1:length(CellList)){
  print(TissueIter)
  TargetCells <- CellList[[TissueIter]]
  CellMatchInd <- which(toupper(colnames(SensProfile)) %in%
                          intersect(toupper(colnames(SensProfile)), TargetCells))
  
  SamNum <- c()
  ClassFrac <- c()
  for(DrugIter in 1:nrow(SensProfile)){
    ObsVec <- SensProfile[DrugIter, CellMatchInd]
    TargetInd <- which(!is.na(ObsVec) & ObsVec != Inf)
    ObsVec <- as.numeric(ObsVec[TargetInd])
    Classes <- ifelse(ObsVec > Cutoff, "High", "Low")
    if(length(table(Classes)) == 2){
      ClassFrac <- c(ClassFrac, min(table(Classes)[2]/table(Classes)[1],
                                    table(Classes)[1]/table(Classes)[2]))
    }else{
      ClassFrac <- c(ClassFrac, 0)
    }
    SamNum <- c(SamNum, length(ObsVec))
  }
  SamNum_Mat <- cbind(SamNum_Mat, SamNum)
  ClassFrac_Mat <- cbind(ClassFrac_Mat, ClassFrac)
}
colnames(SamNum_Mat) <- names(CellList)
rownames(SamNum_Mat) <- rownames(SensProfile)

colnames(ClassFrac_Mat) <- names(CellList)
rownames(ClassFrac_Mat) <- rownames(SensProfile)


which(SamNum_Mat > 40 & ClassFrac_Mat > 0.5, arr.ind = T)

unique(rownames(ClassFrac_Mat)[which(SamNum_Mat > 40 & ClassFrac_Mat > 0.5, arr.ind = T)[,1]])
rownames(ClassFrac_Mat)[which(SamNum_Mat > 40 & ClassFrac_Mat > 0.5, arr.ind = T)[,1]][
  which(duplicated(rownames(ClassFrac_Mat)[which(SamNum_Mat > 40 & ClassFrac_Mat > 0.5, arr.ind = T)[,1]]))]


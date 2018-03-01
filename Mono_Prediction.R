rm(list = ls())
library(glmnet)
library(PharmacoGx)
library(Biobase)
library(preprocessCore)
library(caret)
library(e1071)
library(pROC)
require(PRROC)
library(mRMRe)
library(apcluster)
library(GSVA)
library(MLmetrics)
# library(RSNNS)
#################
# TargetPset <- downloadPSet("GDSC1000")
# SensProfile1 <- summarizeSensitivityProfiles(TargetPset, sensitivity.measure = "auc_recomputed")
# SenseVec <- na.omit(as.numeric(c(SensProfile)))
# Clust <- kmeans(SenseVec,3)
# quantile(SenseVec[which(Clust$cluster == 1)])
# quantile(SenseVec[which(Clust$cluster == 2)])
# quantile(SenseVec[which(Clust$cluster == 3)])

# TargetPset <- downloadPSet("CTRPv2")
# SensProfile2 <- summarizeSensitivityProfiles(TargetPset, sensitivity.measure = "auc_recomputed")
# SenseVec <- na.omit(c(as.numeric(c(SensProfile1)), as.numeric(c(SensProfile2))))
# Clust <- kmeans(SenseVec,3)
# quantile(SenseVec[which(Clust$cluster == 1)])
# quantile(SenseVec[which(Clust$cluster == 2)])
# quantile(SenseVec[which(Clust$cluster == 3)])
################
setwd("~/Desktop/Comparison_ML_DrugResponse/Scripts/")
source("PreProcess.R")

source("DataAug.R")

source("FeatSelect_Var.R")
source("FeatSelect_RF.R")
source("FeatSelect_mRMR.R")
source("FeatSelect_Wrap.R")

source("FeatTransform_Bin.R")
source("FeatTrans_Path.R")
source("FeatTrans_PCA.R")
source("BackgroundAdd.R")

source("Univar_Pred.R")
source("CentClass_Pred.R")
source("GLMnet_Pred.R")
source("RF_Pred.R")
source("GBM_Pred.R")
source("SVM_Pred.R")
source("NNET_Pred.R")
source("Biomarker_Pred.R")
source("NB_Pred.R")

source("CrossVal.R")
source("Sampling.R")

source("ProbCut_Optimize.R")
source("Univar_Optimize.R")
source("GLMnet_Optimize.R")
source("SVM_Optimize.R")
source("RF_Optimize.R")
source("GBM_Optimize.R")

source("Assess.R")
source("Assess_Report.R")
#################
PSet <- "GDSC1000"
Tissue <- "lung"
Drug <- "pha-793887"
MolecularProfile <- "rna"
Measure <- c("auc_recomputed")
HighCutoff <- 0.2
LowCutoff <- 0.2

StudyName <- paste(PSet, Tissue, Drug, MolecularProfile,
                   Measure, HighCutoff, LowCutoff, sep = "_", collapse = "")
#################
ProcessedData <- PreProcess(PSet, Tissue, #haematopoietic_and_lymphoid_tissue
                            Drug, MolecularProfile, #etoposide, zstk474, Cytarabine, Trametinib, selumetinib
                            Measure, #, "ic50_recomputed"
                            HighCutoff, LowCutoff)
###############
# GeneSets <- readRDS("~/Desktop/Comparison_ML_DrugResponse/Pathways_Dataset/c2_cp_reactome_entrez_ProcessPathways.rds")
# FeatFrame_Path <- FeatTrans_Path(ProcessedData$FeatFrame, GeneSets, Method='ssgsea')
# FeatFrame_Bin <- FeatTransform_Bin(ProcessedData$FeatFrame,c(1:ncol(ProcessedData$FeatFrame)),ClustNum=2) #
# FeatFrame_PCA <- FeatTrans_PCA(ProcessedData$FeatFrame)

ObservationVec <- factor(ProcessedData$Observations, ordered = T)
################
################
Models <- c("Univar_Pred", "GLMnet_Pred") #, , "RF_Pred", "CentClass_Pred", "NNET_Pred", "RF_Pred", "SVM_Pred",  "Biomarker_Pred", "NB_Pred"
FeatSelMethods <- c("FeatSelect_Var") #, "Univar"
FeaturFrac <- rep(0.02, length(FeatSelMethods)) # fraction=0.2 is working fine
################
TargetFeatFrame <- ProcessedData$FeatFrame
# aa <- TargetFeatFrame
# for(Iter in 1:10){
#   aa <- rbind(aa, runif(ncol(TargetFeatFrame), 0,0.1))
#   ObservationVec <- c(ObservationVec, 1)
# }
# TargetFeatFrame <- aa
# # for(Iter in 1:50){
# #   aa <- rbind(aa, runif(ncol(TargetFeatFrame), -0.01,0.01))
# #   ObservationVec <- c(ObservationVec, 1)
# # }
# ObservationVec <- factor(ObservationVec, ordered = T)

# BackAdded <- BackgroundAdd(TargetFeatFrame, ObservationVec,
#                            MinRange=0,MaxRnag=0.1, AddClass=1, AddNum=10)
# 
# TargetFeatFrame <- BackAdded$Feat
# ObservationVec <- BackAdded$Observation
###############
ptm <- proc.time()
ResponseList <- Sampling(FeatFrame=TargetFeatFrame,ObsVec=ObservationVec,
                         FeaturFrac,FoldNum=5,Sam_Num=10, Models, FeatSelMethods, Biomarkers=NULL, #c("2064")
                         FixedFeatFrame = NULL, #data.frame(ProcessedData$Tissues)
                         folds = NA, Augment = FALSE, FeatSelect_Num=TRUE,Aug_VarCutoff=0.05) #FeatSelect_Cat=TRUE, 
proc.time() - ptm
################
Assess_Out <- Assess(ResponseList$Predictions,ResponseList$Observations)

print(Assess_Out)
################
Assess_Report(Assess_Out, "~/Desktop/Comparison_ML_DrugResponse/Example/",StudyName)

## Implement deepchecm (Tensor Flow based)


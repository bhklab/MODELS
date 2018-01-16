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


source("FeatTransform_Bin.R")
source("FeatSelect.R")
source("FeatTrans_Path.R")
source("FeatTrans_PCA.R")

source("Univar_Pred.R")
source("Univar_CV.R")
source("CentClass_Pred.R")
source("CentClass_CV.R")
source("GLMnet_CV.R")
source("GLMnet_Pred.R")
source("RF_Pred.R")
source("RF_CV.R")
source("GBM_Pred.R")
source("GBM_CV.R")
source("SVM_Pred.R")
source("SVM_CV.R")
source("NNET_Pred.R")

source("CrossVal.R")

source("Univar_Optimize.R")
source("GLMnet_Optimize.R")
source("SVM_Optimize.R")
source("RF_Optimize.R")
source("GBM_Optimize.R")

source("Assess.R")
#################
ProcessedData <- PreProcess(PSet ="GDSC1000", Tissue="lung", #haematopoietic_and_lymphoid_tissue
                            Drug="Cytarabine", MolecularProfile ="rna", #etoposide, zstk474, Cytarabine
                            Measure=c("auc_recomputed", "ic50_recomputed"),
                            HighCutoff= c(0.2,1), LowCutoff= c(0.2,1))
###############
# GeneSets <- readRDS("~/Desktop/Comparison_ML_DrugResponse/Pathways_Dataset/c2_cp_reactome_entrez_ProcessPathways.rds")
# FeatFrame_Path <- FeatTrans_Path(ProcessedData$FeatFrame, GeneSets, Method='ssgsea')
# FeatFrame_Bin <- FeatTransform(ProcessedData$FeatFrame,c(1:ncol(ProcessedData$FeatFrame)),ClustNum=2) #
# FeatFrame_PCA <- FeatTrans_PCA(ProcessedData$FeatFrame)

ObservationVec <- factor(ProcessedData$Observations, ordered = T)
###############
ptm <- proc.time()
TargetFeatFrame <- ProcessedData$FeatFrame
ResponseList <- CrossVal(FeatFrame=TargetFeatFrame,ObsVec=ObservationVec,
                         FeaturNum = floor(ncol(TargetFeatFrame)/2),
                         FoldNum=10,BS_Num=1,
                         FeatSelect_Num=TRUE,FeatSelect_Cat=FALSE)
proc.time() - ptm
################
Assess_Out <- Assess(ResponseList$Predictions,
       as.numeric(ResponseList$Observations))

print(Assess_Out)
################
boxplot(Assess_Out$MCC, ylab="MCC")
boxplot(Assess_Out$AUC_ROC, ylab = "AUC_ROC")
boxplot(Assess_Out$AUC_PR, ylab = "AUC_PR")
boxplot(Assess_Out$F1, ylab = "F1 score")


## Implement deepchecm (Tensor Flow based)



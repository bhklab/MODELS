############################# CrossVal is a function for cross validation 
############################# Input variables of this function are as follows:
############################# 1) FeatFrame: Feature frame (rows as samples and columns as features) 
############################# 2) ObsVec: Observed classess
############################# 3) FeaturFrac: Fraction of features to be kept in output of each feature selection approach
############################# 4) FoldNum: Number of Folds for cross validation
############################# 5) Models: Names of predictive models to be used for predicting observed classes
############################# 6) FeatSelMethods: Names of feature selection approaches (in order)
############################# 7) Biomarkers: Biomarkers (for example vector of gene names) if user want to predict observed classes using predefined biomarkers in a multivariate logistic regression model
############################# 8) FixedFeatFrame: Frame of features whcih are fixed and will not be processed using feature selection approaches
############################# 9) Folds: Pre-defined list of samples to be used in each fold of cross validation
############################# 10) Augment: Logical variable to determine use of data augmentation for imbalanced observed classes (within training set)
############################# 11) FeatSelect_Num: Logical variable to determine if variables are numerical
############################# 12) Aug_VarCutoff: Maximum variance to be used for data augmentation

CrossVal <- function(FeatFrame, ObsVec, FeaturFrac, FoldNum, Models,FeatSelMethods,Biomarkers=NULL, FixedFeatFrame = NULL,
                     Folds = NA, Augment=FALSE,FeatSelect_Num=FALSE, Aug_VarCutoff){
  if(is.na(Folds)){
    Folds <- createFolds(y=ObsVec, k = FoldNum, list = TRUE, returnTrain = TRUE)
  }

  
  PredMat <- c()
  ObservVec <- c()
  for(Folditer in 1:FoldNum){
    print(paste("Fold ", Folditer, sep = "", collapse = ""))
    InputInd <- Folds[[Folditer]]
    ObservVec <- c(ObservVec, ObsVec[-InputInd])
    
    TrainFeat <- FeatFrame[InputInd,]
    TrainObs <- ObsVec[InputInd]
    TestFeat <- FeatFrame[-InputInd,]
    ################### Tissue feature separation
    ################### Feature selection
    TargetFeatures <- FeatSelect_Wrap(TrainFeat, TrainObs, FeaturFrac,FeatSelect_Num, FeatSelMethods)
    if("Biomarker_Pred" %in% Models){
      TargetFeatures <- unique(c(TargetFeatures, which(gsub("X", "", colnames(TrainFeat)) %in% Biomarkers)))
    }
    ###################
    TrainFeat <- TrainFeat[,TargetFeatures]
    TestFeat <- TestFeat[,TargetFeatures]
    #################### Data augmentation
    if(Augment){
      AugList <- DataAug(TrainFeat, TrainObs, Aug_VarCutoff)
      TrainFeat <- NULL
      TrainObs <- NULL
      TrainFeat <- AugList$Feat
      TrainObs <- AugList$Obs
    }
    ###################
    if(!is.null(FixedFeatFrame)){
      FixedFrame_Train <- data.frame(FixedFeatFrame[InputInd,])
      names(FixedFrame_Train) <- names(FixedFeatFrame)
      TrainFeat <- cbind(TrainFeat, FixedFrame_Train)
      
      FixedFrame_Test <- data.frame(FixedFeatFrame[-InputInd,])
      names(FixedFrame_Test) <- names(FixedFeatFrame)
      TestFeat <- cbind(TestFeat, FixedFrame_Test)
    }
    ################### Predictive models
    PredMat_Aux <- c()           
    for(ModelIter in 1:length(Models)){
      MatchedFun <- match.fun(Models[ModelIter])
      if(Models[ModelIter] == "Univar_Pred"){
        PredVec <- MatchedFun(TrainFeat, TrainObs, TestFeat)$PredVal
      }else if(Models[ModelIter] == "Biomarker_Pred"){
        PredVec <- MatchedFun(TrainFeat, TrainObs, TestFeat, Biomarkers)
      }else{
        PredVec <- MatchedFun(TrainFeat, TrainObs, TestFeat)
      }
      
      
      print(Models[ModelIter])
      PredMat_Aux <- cbind(PredMat_Aux, PredVec)
    }
    ###################
    Ensemble <- unlist(apply(PredMat_Aux, 1, function(X){names(table(X))[
      which(table(X) == max(table(X)))[sample.int(length(which(table(X) == max(table(X)))),1)]]}))
    ###################
    PredMat <- rbind(PredMat, cbind(PredMat_Aux, Ensemble))
  }
  #############
  colnames(PredMat) <- c(gsub("_Pred", "", Models),"Ensemble")
  print(dim(PredMat))
  #############
  PredList <- list(PredMat=PredMat, ObservVec=ObservVec)
  return(PredList)
}


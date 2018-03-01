############################# CrossVal is a function for cross validation 
############################# Input variables of this function are as follows:
############################# 1) FeatFrame: Feature frame (rows as samples and columns as features) 
############################# 2) ObsVec: Observed classess
############################# 3) FeaturFrac: Fraction of features to be kept in output of each feature selection approach
############################# 4) FoldNum: Number of Folds for cross validation
############################# 5) Sam_Num: Number of sampling (In each sampling order of samples will be changed)
############################# 6) Models: Names of predictive models to be used for predicting observed classes
############################# 7) FeatSelMethods: Names of feature selection approaches (in order)
############################# 8) Biomarkers: Biomarkers (for example vector of gene names) if user want to predict observed classes using predefined biomarkers in a multivariate logistic regression model
############################# 9) FixedFeatFrame: Frame of features whcih are fixed and will not be processed using feature selection approaches
############################# 10) Folds: Pre-defined list of samples to be used in each fold of cross validation
############################# 11) Augment: Logical variable to determine use of data augmentation for imbalanced observed classes (within training set)
############################# 12) FeatSelect_Num: Logical variable to determine if variables are numerical
############################# 13) Aug_VarCutoff: Maximum variance to be used for data augmentation

Sampling <- function(FeatFrame, ObsVec, FeaturFrac, FoldNum, Sam_Num, Models,FeatSelMethods,Biomarkers,FixedFeatFrame=NULL,
                     folds,Augment, FeatSelect_Num,Aug_VarCutoff){
  ############### Checking dimension of feature matrix and length of observation vector
  if(length(ObsVec) != nrow(FeatFrame)){
    stop("Number of rows of feature matrix should be the same as length of observation vector.")
  }
  ###############
  if(!is.null(FixedFeatFrame)){
    if(nrow(FixedFeatFrame) != nrow(FeatFrame)){
      stop("Number of rows of fixed features matrix should be the same as number of rows of feature matrix.")
    }
    ##############
    for(FeatIter in 1:ncol(FixedFeatFrame)){
      if(length(unique(FixedFeatFrame[,FeatIter])) < 2){
        stop("You have to remove fixed features with less than 2 levels.")
      }
    }
  }
  ###############
  PredList <- list()
  Observlist <- list()
  for(Samiter in 1:Sam_Num){
    print(paste("Sam ", Samiter, sep = "", collapse = ""))
    
    PredCV <- CrossVal(FeatFrame, ObsVec, FeaturFrac,
                       FoldNum, Models,FeatSelMethods,Biomarkers,FixedFeatFrame,
                       folds, Augment, FeatSelect_Num,Aug_VarCutoff) 
    
    PredList[[Samiter]] <- data.frame(PredCV$PredMat)
    Observlist[[Samiter]] <- PredCV$ObservVec
  }
  names(PredList) <- paste(rep("Sam", Sam_Num), seq(1,Sam_Num), sep = "_")
  names(Observlist) <- paste(rep("Sam", Sam_Num), seq(1,Sam_Num), sep = "_")
  #############
  ResponseList <- list(Predictions=PredList, Observations=Observlist)
  #############
  
  return(ResponseList)
}


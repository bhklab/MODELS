CrossVal <- function(FeatFrame, ObsVec, FeaturNum, FoldNum, BS_Num,FeatSelect_Num=FALSE,FeatSelect_Cat=FALSE){
  
  Models <- c("GLMnet_Pred", "RF_Pred", "SVM_Pred", "NNET_Pred", "CentClass_Pred")
  
  PredList <- list()
  for(BSiter in 1:BS_Num){
    print(paste("BS ", BSiter, sep = "", collapse = ""))
    folds <- createFolds(y=ObsVec, k = FoldNum, list = TRUE, returnTrain = TRUE)
    
    PredMat <- c()
    ObservVec <- c()
    for(Folditer in 1:FoldNum){
      print(paste("Fold ", Folditer, sep = "", collapse = ""))
      InputInd <- folds[[Folditer]]
      ObservVec <- c(ObservVec, ObsVec[-InputInd])
      # TrainFeat <- matrix(FeatFrame[InputInd,], ncol = ncol(FeatFrame))
      TrainFeat <- FeatFrame[InputInd,]
      TrainObs <- ObsVec[InputInd]
      #################### Feature selection
      FeatList <- FeatSelect(FeatFrame,InputInd,TrainFeat, TestFeat, FeaturNum,
                             FeatSelect_Num,FeatSelect_Cat)
      TrainFeat <- FeatList$Training
      TestFeat <- FeatList$Testing

      
      # threads <- get.thread.count()
      # set.thread.count(threads)
      # 
      # FeatFrame_mRMR <- data.frame(cbind(TrainFeat, TrainObs))
      # feature_data <- mRMR.data(data = FeatFrame_mRMR)#data.frame(TrainFeat))

      # filter <- mRMR.classic("mRMRe.Filter", data = feature_data,
      #                        target_indices = ncol(FeatFrame_mRMR),
      #                        feature_count = FeaturNum)
      # TargetFeatures <- solutions(filter)$`1`
      
      # TargetFeatures <- c(1:1000)
      
      # TrainFeat <- TrainFeat[,TargetFeatures]

      # control <- rfeControl(functions=rfFuncs, method="cv", number=10)
      # results <- rfe(FeatFrame_mRMR[,(1:(ncol(FeatFrame_mRMR)-1))], FeatFrame_mRMR[,ncol(FeatFrame_mRMR)],
      #                sizes=c(1:FeaturNum), rfeControl=control)
      # print(results)
      # predictors(results)
      # print(results$fit$importance[,"MeanDecreaseGini"]) #
      # TargetFeatures <- sort(results$fit$importance[,"MeanDecreaseAccuracy"], decreasing = T, index.return=T)[[2]][1:FeaturNum]
      # stop("caret yess :)")
      # print(TargetFeatures)
      ####################
      # TestFeat <- matrix(FeatFrame[-InputInd,TargetFeatures], ncol = length(TargetFeatures))
      # TrainFeat <- TrainFeat[,TargetFeatures]
      # TestFeat <- FeatFrame[-InputInd,na.omit(TargetFeatures)]
      
      #print("Implementing univariate model")
      ####################
      if("CentClass_Pred" %in% Models){
        PredMat_Aux <- CentClass_Pred(TrainFeat, TrainObs, TestFeat)
        Models_2 <- Models[-which(Models == "CentClass_Pred")]
      }else{
        PredMat_Aux <- c()
        Models_2 <- Models
      }
      
      ###################
      Uni_PredList <- Univar_Pred(TrainFeat, TrainObs,TestFeat)
      Uni_Pred <- Uni_PredList$PredVal
      # SelectedFeat <- c(1:ncol(TrainFeat))
      SelectedFeat <- Uni_PredList$GoodFeat
      print(length(SelectedFeat))
      ###############
      threads <- get.thread.count()
      set.thread.count(threads)

      train_mRMR <- TrainFeat[,SelectedFeat]
      train_mRMR$Obs <- TrainObs
      FeatFrame_mRMR <- data.frame(train_mRMR)
      
      feature_data <- mRMR.data(data = FeatFrame_mRMR)#data.frame(TrainFeat))
      
      filter <- mRMR.classic("mRMRe.Filter", data = feature_data,
                             target_indices = which(colnames(train_mRMR) == "Obs"),
                             #solution_count = 10,
                             feature_count = floor(0.5*length(SelectedFeat)))
      # print(solutions(filter))
      SelectedFeat <- SelectedFeat[unlist(solutions(filter))]
      # print(SelectedFeat)
      # print(names(TrainFeat[,SelectedFeat]))
      ##############
      
      PredMat_Aux <- cbind(PredMat_Aux, Uni_Pred)
      for(ModelIter in 1:length(Models_2)){
        MatchedFun <- match.fun(Models[ModelIter])
        PredMat_Aux <- cbind(PredMat_Aux, MatchedFun(TrainFeat[,SelectedFeat], TrainObs, TestFeat[,SelectedFeat]))
        
      }

      Ensemble <- unlist(apply(PredMat_Aux, 1, function(X){names(table(X))[
        which(table(X) == max(table(X)))[sample.int(length(which(table(X) == max(table(X)))),1)]]}))
      PredMat <- rbind(PredMat, cbind(PredMat_Aux, Ensemble))
    }
    #############
    colnames(PredMat) <- c("CentClass","Univariate", gsub("_Pred", "", Models_2),
                           "Ensemble")
    
    PredList[[BSiter]] <- data.frame(PredMat)
  }
  names(PredList) <- paste(rep("BS", BS_Num), seq(1,BS_Num), sep = "_")
  ##############
  ResponseList <- list(Predictions=PredList, Observations=ObservVec)
  #############
  
  return(ResponseList)
}


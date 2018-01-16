Vaidation <- function(FeatTrans, FeatSelect, Predictive,TrainFrame,TestFrame,TrainObs, TestObs){
  
  FeatTrans_Fun <- match.fun(FeatTrans)
  FeatFrame_Train <- FeatTrans_Fun(TrainFrame)
  FeatFrame_Test <- FeatTrans_Fun(TestFrame)
  
  FeatSelect_Fun <- match.fun(FeatSelect)
  SelectedInd <- FeatSelect_Fun(FeatFrame_Train, TrainObs)
  FeatFrame_Train_Sel <- FeatFrame_Train[,SelectedInd]
  FeatFrame_Test_Sel <- FeatFrame_Test[,SelectedInd]
  
  Predictive_Fun <- match.fun(Predictive)
  PredictedVec <- Predictive_Fun(FeatFrame_Train_Sel, TrainObs, FeatFrame_Test_Sel)
  
  return(PredictedVec)
}
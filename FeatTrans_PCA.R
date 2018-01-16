FeatTrans_PCA <- function(FeatFrame){
  FeatMat <- as.matrix(FeatFrame)
  Feat.PCA <- prcomp(FeatMat,center = TRUE,scale. = TRUE) 
  
  PCAFrame <- as.data.frame(Feat.PCA$x)
  return(PCAFrame)
}
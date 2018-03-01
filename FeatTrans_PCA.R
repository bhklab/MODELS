############################# FeatTrans_Path is a feature transformation function to reduce dimensionality using principle component analysis
############################# Input variables of this function are as follows:
############################# 1) FeatFrame: Feature frame (rows as samples and columns as features) 

FeatTrans_PCA <- function(FeatFrame){
  FeatMat <- as.matrix(FeatFrame)
  Feat.PCA <- prcomp(FeatMat,center = TRUE,scale. = TRUE) 
  
  PCAFrame <- as.data.frame(Feat.PCA$x)
  return(PCAFrame)
}
############################# FeatTrans_Path is a feature transformation function to transform gene expression profile to encirhment of pathways
############################# Input variables of this function are as follows:
############################# 1) FeatFrame: Feature frame (rows as samples and columns as features) 
############################# 2) GeneSets: Set of pathways to implemenet pathway enrichment for
############################# 3) Method: Method of pathway enrichment available in GSVA package

FeatTrans_Path <- function(FeatFrame, GeneSets, Method='ssgsea'){
  
  FeatMat <- t(as.matrix(FeatFrame))
  rownames(FeatMat) <- as.numeric(gsub("X", "", colnames(FeatFrame)))
  ###########
  GSVA_TMP <- invisible(gsva(FeatMat, GeneSets, method=Method))
  rownames(GSVA_TMP) <- paste("Path", rownames(GSVA_TMP), sep = "_")
  GSVAFrame <- as.data.frame(t(GSVA_TMP))
  
  return(GSVAFrame)
}
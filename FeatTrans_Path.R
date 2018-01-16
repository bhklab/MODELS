FeatTrans_Path <- function(FeatFrame, GeneSets, Method='ssgsea'){
  
  FeatMat <- t(as.matrix(FeatFrame))
  rownames(FeatMat) <- as.numeric(gsub("X", "", colnames(FeatFrame)))
  ###########
  GSVA_TMP <- invisible(gsva(FeatMat, GeneSets, method=Method))
  rownames(GSVA_TMP) <- paste("Path", rownames(GSVA_TMP), sep = "_")
  GSVAFrame <- as.data.frame(t(GSVA_TMP))
  
  return(GSVAFrame)
}
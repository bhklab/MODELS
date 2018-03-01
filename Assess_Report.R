############################# Assess_Report is a post-processing function for reporting the assessment of predictive models
############################# Input variables of this function are as follows:
############################# 1) Assess_Out: Output of Assess function 
############################# 2) OutDir: Path to the output directory for saving the report of the results

Assess_Report <- function(Assess_Out, OutDir, StudyName){
  
  
  for(MeasIter in 1:length(Assess_Out)){
    AssessMat <- Assess_Out[[MeasIter]]
    MeasName <- names(Assess_Out)[MeasIter]

    Vioplot_Vec <- as.vector(AssessMat)
    dat <- data.frame(x= c(unlist(lapply(c(1:ncol(AssessMat)), function(Iter){
      rep(colnames(AssessMat)[Iter], nrow(AssessMat))}))),
                      y= as.numeric(Vioplot_Vec),
                      levels = colnames(AssessMat))
    
    pdf(paste(OutDir, StudyName, "_", MeasName, "_Vioplot.pdf",
              sep = "", collapse = ""),
        onefile = F, width = (2+0.5*ncol(AssessMat)), height = 4)
    par(mar=c(2,6,4,2)+0.1,mgp=c(3,1,0))
    myplot <- ggplot(dat, aes(x=factor(x, levels=unique(x)), y=y)) +
      geom_violin(aes(fill = factor(x)), adjust = 3.5) +
      theme_bw(base_size = 20) +
      labs(x = "", y = names(Assess_Out)[MeasIter]) +
      theme(legend.position = "none",axis.text.x=element_text(angle=90, hjust=1, vjust = 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black")) 
    print(myplot)
    # par(mar=c(7,6,2,1)+0.1,mgp=c(4.5,1,0))
    # boxplot(AssessMat,  ylab = MeasName, las=2, 
    #         ylim=c(floor(min(na.omit(AssessMat))*100)/100,
    #                min(1, ceiling(max(na.omit(AssessMat))*100)/100)),
    #         cex.lab = 1.5, cex.axis = 1.5, frame.plot=FALSE, xaxt='n')
    # axis(side=1, pos=floor(min(AssessMat)*100)/100, lwd.ticks=0, 
    #      labels = colnames(AssessMat), at = seq(1,ncol(AssessMat)), las = 2)
    # abline(h=0)
    dev.off()
  }
  
}




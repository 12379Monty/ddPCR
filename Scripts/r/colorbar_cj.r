# ------------------------------------------------------------------------------------
# 2/20/2013
# My own version of colorbar
# Script adapted from http://www.colbyimaging.com/wiki/statistics/color-bars
#
# Use this version to be in full concordance with the colormap scheme derived
# from customcolormap.r and num2CMP.r
#
# [USAGE]
# [one-sided]
#   colorbar_cj(c("white", "red"), 64, 1, c(0,100), nticks=5)
# [two-sided]
#   colorbar_cj(c("red", "white", "green"), 128, 0.9, c(-5,0,5), nticks=5)
#   colorbar_cj(c("red", "white", "green"), 128, 0.9, c(-5,0,5), "yLab", seq(1,9,length.out=5), nticks=5)
#
# by CJ Ku
# ------------------------------------------------------------------------------------

# Function to plot color bar
colorbar_cj <- function(basecolor, ncolor, rcolor, ylim, nticks=5, ...) {
   
   varargin  = list(...)
   dbmode    = load_varargin("dbmode", FALSE, varargin)
   title     = load_varargin("title", "", varargin)
   FSIZE     = load_varargin("FSIZE", c(2,3), varargin)
   yLab      = load_varargin("yLab", NULL, varargin)
   fmt       = load_varargin("fmt", "x11", varargin)
   savefig   = load_varargin("savefig", FALSE, varargin)
   figurefilename = load_varargin("figurefilename", NA, varargin)
            
   CMP0 = colorRampPalette(basecolor)
   bartype = length(basecolor)-1 # one-sided/two-sided
   
   if (dbmode) {
       browser()
   }
   
   if (savefig) {
       if (is.na(figurefilename)) {
           figurefilename = file.path(
               Wdir$ana, "other",
               paste("colorbar_cj", paste("bartype", bartype, sep=""),
                     "png", sep="."))
       }         
       maiAll = c(0.1,0.1,0.1,0.1)
       omiAll = c(0.3,0.25,0.3,0.70)
       
       require(grDevices) 
       png(file = figurefilename, width = FSIZE[1], height = FSIZE[2],
           units = "in", res = 600, pointsize = 8)
       par(mai = maiAll, omi = omiAll)
       
   } else if (fmt=="x11") {
       x11(FSIZE[1], FSIZE[2])
       par(mar=c(2,2,1,5))
       
   } else if (fmt=="inset") {
       inset.xmin = load_varargin("inset.xmin", 0.82, varargin)
       inset.xmax = load_varargin("inset.xmax", 0.85, varargin)
       inset.ymin = load_varargin("inset.ymin", 0.7, varargin)
       inset.ymax = load_varargin("inset.ymax", 0.85, varargin)
       par(fig = c(inset.xmin, inset.xmax, inset.ymin, inset.ymax),
           mar=c(0,0,0,0), new=TRUE)
   } else {
       par(mar=c(2,2,1,5))
   }
   
   plot(c(0,1), c(ylim[1],ylim[length(ylim)]), type='n', bty='n', xaxt='n',
        xlab='', yaxt='n', ylab='', main="")
   rect(0, ylim[1], 1, ylim[length(ylim)], col="black")
   if (is.null(yLab)) {
       axis(side=4, las=1, at=seq(ylim[1],ylim[length(ylim)],length.out=nticks),
            labels=seq(ylim[1],ylim[length(ylim)],length.out=nticks),
            cex.axis=0.6)
   } else {
       axis(side=4, las=1, at=seq(ylim[1],ylim[length(ylim)],length.out=nticks),
            labels=yLab, cex.axis=0.6)
   }
   axis(side=2, at=0.5, labels = title, cex.axis = 0.7, tick = F)
   
   if (bartype==1) {
      ticks  = seq(ylim[1], ylim[2], len=nticks)   
   }
   else if (bartype==2) {
      nticks = ceiling(nticks/2)
      ticks  = unique(c(seq(ylim[1], ylim[2], len=nticks),
                        seq(ylim[2], ylim[3], len=nticks)))
   }
   axis(outer=TRUE, side=4, signif(ticks,3), las=2, cex.axis=0.75)
   
   if (bartype==1) { 
      CMP = CMP0(ncolor)
      for (i in 1:length(CMP)) {
         yL = ((i-1)/length(CMP))^rcolor*(ylim[2]-ylim[1]) + ylim[1]
         yU = (i/length(CMP))^rcolor*(ylim[2]-ylim[1]) + ylim[1]         
         rect(0, yL, 1, yU, col=CMP[i], border=NA)
         #print(c(yL,yU))         
         #y = (i-1)/scale + min
         #rect(0, y, 1, y+1/scale, col=lut[i], border=NA)
      }
   } 
   else if (bartype==2) {
      if (ncolor%%2==1) {
         ncolor = ncolor-1
      }
      CMP = CMP0(ncolor)
      ytick1 = seq(0, ncolor/2)
      for (i in 1:(length(CMP)/2)) {
         yL = ((i-1)/(length(CMP)/2))^rcolor*(ylim[2]-ylim[1]) + ylim[1]
         yU = (i/(length(CMP)/2))^rcolor*(ylim[2]-ylim[1]) + ylim[1]         
         rect(0, yL, 1, yU, col=CMP[i], border=NA)
         
         yL = ((i-1)/(length(CMP)/2))^(1/rcolor)*(ylim[3]-ylim[2]) + ylim[2]
         yU = (i/(length(CMP)/2))^(1/rcolor)*(ylim[3]-ylim[2]) + ylim[2]         
         rect(0, yL, 1, yU, col=CMP[i+length(CMP)/2], border=NA)
      }
   }
   
   if (savefig) {
      dev.off()
      print(paste("> Saving colorbar figure in", figurefilename, sep=" "))
   }
}

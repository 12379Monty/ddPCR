# ----------------------------------------------------------------------------------------------
# 2/15/2013
# Convert a set of numbers to colors based on an initial colormap CMP with depth n, variation r
#
# [USAGE]
# 
#
# [1-sided]
# N = 16;  r = 1.2;  xlim = c(1,2);
# x = seq(0.5,2.5,by=0.02)
# CMP0 = colorRampPalette(c("blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red"))
# xcol = num2CMP(x, xlim, CMP0, N, r, "dbmode", TRUE)
#
# [2-sided]
# N = 129;  r = 1.0; xlim = c(-1,0,3);
# x = seq(-2, 2.5, by=0.01)
# CMP0 = colorRampPalette(c("green", "white", "red"))
# xcol = num2CMP(x, xlim, CMP0, N, r, "dbmode", TRUE)
#
# by CJ Ku
# ----------------------------------------------------------------------------------------------


num2CMP <- function(x, xlim, basecolor, N, r, ...) {
   
   varargin  = list(...)
   dbmode    = load_varargin("dbmode",FALSE,varargin)      
   
   if (dbmode) {
      browser()
   }
   
   CMP = colorRampPalette(basecolor)
   
   if (length(xlim)==2) {
      # one-sided colormap
      # typically set N = 2^M
      
      xL = xlim[1]
      xU = xlim[2]
      x[x<xL] = xL
      x[x>xU] = xU
      y = ((x-xL)/(xU-xL))^r
      z = sapply(y, FUN=function(q,n) { return(max(1,ceiling(n*q))) }, N)
      return(CMP(N)[z])
   }
   else if (length(xlim)==3) {
      # two-sided colormap
      # typically set N = 2*(2^M)+1 and CMP0 = 3-colors vector
      
      if (N%%2==0) { N = N+1 }
      N1 = (N-1)/2
            xL = xlim[1]
      xM = xlim[2]
      xU = xlim[3]
      x[x<xL] = xL
      x[x>xU] = xU
      
      y = numeric(length=length(x))      
      y[x<xM]  = ((x[x<xM]-xL)/(xM-xL))^r
      y[x<xM]  = 1 + floor(N1*y[x<xM])
      y[x>=xM] = ((x[x>=xM]-xM)/(xU-xM))^(1/r)
      y[x>=xM] = N1+1+floor(N1*y[x>=xM])
      return(CMP(N)[y])
   }
   else {
      stop("xlim should contain either 2 or 3 element")
   }
}
issuperstr <- function(x,y, ...) {
  
   varargin  = list(...)
   dbmode    = load_varargin("dbmode", FALSE, varargin)
         
   Lx = length(x)
   Ly = length(y)
   
   if (Lx==1 & Ly==1) {
      nx = nchar(x)
      ny = nchar(y)
      
      if (ny>nx) {
         return(FALSE)
      }
      else {
         pos = logical(length = nx-ny+1)
         for (i in seq(1,nx-ny+1)) {
            pos[i] = (y==substring(x,i,i+ny-1))      
         }
         z = (length(which(pos))>0)
      }
   }
   
   else if (Lx>1 & Ly==1) {
      z = sapply(x, issuperstr, y)
   }
   
   else if (Lx==1 & Ly>1) {
      z = sapply(y, issubstr, x)
   }
   
   else {
      z = matrix(FALSE, nrow=Lx, ncol=Ly)
      for (i in seq(1,Ly)) {
         temp = sapply(x, issuperstr, y[i])
         dim(temp) <- c(Lx,1)
         z[,i] = temp
      }      
   }
   return(z)
}
issubstr <- function(x,y) {
   
   Lx = length(x)
   Ly = length(y)
   
   if (Lx==1 & Ly==1) {
      nx = nchar(x)
      ny = nchar(y)
      
      if (nx>ny) {
         return(FALSE)
      }
      else {
         pos = logical(length = ny-nx+1)
         for (i in seq(1,ny-nx+1)) {
            pos[i] = (x==substring(y,i,i+nx-1))      
         }    
         z = (length(which(pos))>0)
      } 
   }
   else if (Lx>1 & Ly==1) {
      z = sapply(x, issubstr, y)
   }
   else if (Lx==1 & Ly>1) {
      z = sapply(y, issuperstr, x)
   }
   else {
      z = matrix(FALSE, nrow=Lx, ncol=Ly)
      for (i in seq(1,Ly)) {
         temp = sapply(x, issubstr, y[i])
         dim(temp) <- c(Lx,1)
         z[,i] = temp
      } 
   }
   return(z)
}

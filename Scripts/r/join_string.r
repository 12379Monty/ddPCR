join_string <- function(vstr, sepstr) {
  
  vstr = vstr[which(vstr!="" & nchar(vstr)>0)]
  if (length(vstr)==0) {
     outstr=""
  }
  else {
     for (i in seq(1,length(vstr))) {
        if (i==1) {
           outstr = vstr[i]
        }
        else {
           outstr = paste(outstr, vstr[i], sep = sepstr)
        }   
     }
  }     
  return(outstr)
}
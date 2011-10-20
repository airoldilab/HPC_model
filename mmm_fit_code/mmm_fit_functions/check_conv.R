# Script to check convergence for parameters using reltol measure
# Returns logical value

check.conv <- function(old.param.vec,new.param.vec,
                       reltol=sqrt(.Machine$double.eps)){
  rel.diff <- abs(old.param.vec-new.param.vec)/abs(old.param.vec)
  if(all(rel.diff < reltol)){conv <- TRUE}
  else{conv <- FALSE}
  return(conv)
}

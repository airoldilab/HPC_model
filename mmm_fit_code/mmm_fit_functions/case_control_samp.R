# Function to implement case control likelihood sampling for
# both sides of HPD model

# Here X is the covariate matrix
# X.active is the rows of the covariate matrix pertaining to active entries
# N.samp is the total number of rows of the covariate matrix to sample
# N.tot is the total rows of the covariate matrix
# active.index is a vector of indices of the active rows in the covariate matrix
# tot.index is a vector of all the row indices 

case.control.samp <- function(X,X.active,N.samp,active.index,
                              doc.length.vec=NULL){

  tot.index <- rownames(X)
  N.tot <- nrow(X)
  N.active <- length(active.index)
  N.inactive <- N.tot - N.active

  # Make sure not requesting more samples than total items
  if(N.samp > N.tot){stop("User requested to sample more documents than in dataset")}

  # Three different strategies depending on relative size of N.active,N.samp,N.tot:
  
  # If active docs more than half the total, just do random sample
  if(N.active >= 0.5*N.tot){
    index.samp <- sample(tot.index,size=N.samp,replace=FALSE)
    # Grab sampled indices
    X <- X[index.samp,,drop=FALSE]
    # Figure out which of the sampled indices are active
    active.samp <- active.index[active.index %in% index.samp]
    X.active <- X.active[active.samp,,drop=FALSE]
    # Don't need to weight samples---can just proceed as normal
    case.control.correct <- FALSE
    # Just need to record multiplicative constant for likelihood
    like.mult <- N.tot/N.samp
    
    # If active docs less than half the total but more than half of
    # what we want to sample, sample from both active and
    # inactive groups
    } else if(all(N.active > 0.5*N.samp, N.active < 0.5*N.tot)){
      N.active.samp <- trunc(N.samp/2)
      weight.active.samp <- N.active/N.active.samp
      N.inactive.samp <- N.samp - N.active.samp
      weight.inactive.samp <- N.inactive/N.inactive.samp
      inactive.index <- tot.index[!(tot.index %in% active.index)]
      active.samp <- sample(active.index,size=N.active.samp,replace=FALSE)
      inactive.samp <- sample(inactive.index,size=N.inactive.samp,replace=FALSE)
      index.samp <- c(active.samp,inactive.samp)
      # Grab sampled docs
      X <- X[index.samp,,drop=FALSE]
      X.active <- X.active[active.samp,,drop=FALSE]
      case.control.correct <- TRUE
      like.mult <- NULL
      
    # Else if the number of active documents less than half what you
    # want to sample, keep them all and only sample inactive
    } else {
      N.active.samp <- N.active
      N.inactive.samp <- N.samp - N.active.samp
      weight.active.samp <- 1
      weight.inactive.samp <- N.inactive/N.inactive.samp
      inactive.index <- tot.index[!(tot.index %in% active.index)]
      active.samp <- active.index
      inactive.samp <- sample(inactive.index,size=N.inactive.samp,replace=FALSE)
      index.samp <- c(active.samp,inactive.samp)
      # Grab sampled docs
      X <- X[index.samp,]
      case.control.correct <- TRUE
      like.mult <- NULL
    }

  # Get corrected doc.length.vec
  if(!is.null(doc.length.vec)){doc.length.vec <- doc.length.vec[index.samp]}
  # If weights for 
  if(case.control.correct){
    # Cache column sums of X for gradient calculation with weights
    wX <- X
    wX[active.samp,] <- weight.active.samp*wX[active.samp,]
    wX[inactive.samp,] <- weight.inactive.samp*wX[inactive.samp,]
    # Need to separate out the cases where rows of X must be scaled by doc.length.vec
    if(is.null(doc.length.vec)){X.col.sums <- colSums(wX)
    } else {X.col.sums <- colSums(doc.length.vec*wX)}
  } else {
    # Cache column sums of X for gradient calculation without weights
    if(is.null(doc.length.vec)){X.col.sums <- colSums(X)
    } else {X.col.sums <- colSums(doc.length.vec*X)}
  }

  out.list <- list(X=X,X.active=X.active,X.col.sums=X.col.sums,
                   case.control.correct=case.control.correct,
                   active.samp=active.samp,index.samp=index.samp,
                   like.mult=like.mult)

  # Only need extra information if don't have equal weights in
  # likelihood
  if(case.control.correct){
    out.list$weight.active.samp <- weight.active.samp
    out.list$weight.inactive.samp <- weight.inactive.samp
    out.list$inactive.samp <- inactive.samp
  }

  # Return corrected doc.length.vec if used
  if(!is.null(doc.length.vec)){out.list$doc.length.vec <- doc.length.vec}
  
  return(out.list)
}

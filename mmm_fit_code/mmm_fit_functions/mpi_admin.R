# Script to open or close down MPI

mpi.admin <- function(state="open"){

  # Open up MPI
  if(state=="open"){

    # Load Rmpi
    if (!is.loaded("mpi_initialize")) {
      library(Rmpi)
    }
    
    .Last <- function() {
      if (is.loaded("mpi_initialize")) {
        ## if (mpi.comm.size()>0) {
        ##   mpi.close.Rslaves()
        ## }
        .Call("mpi_finalize")
      }
    }

    # Get node rank
    mpi.root <- 0
    mpi.size <- mpi.comm.size(0)
    mpi.rank <- mpi.comm.rank(0)

    # Make sure slaves actually load
    if (mpi.size < 2) {
      print("More slave processes are required.")
      mpi.quit()
    }

    out.list <- list(mpi.root=mpi.root,mpi.size=mpi.size,mpi.rank=mpi.rank)
    return(out.list)
  }


  # Close down MPI
  if(state=="close"){
    mpi.quit(save="no")
    #.Call("mpi_finalize")
  }
}







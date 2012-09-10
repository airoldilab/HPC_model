# Testing out rmpi. In this version of the example, I load up the data
# in the master node and broadcast it out to the slaves

# Load Rmpi
if (!is.loaded("mpi_initialize")) {
    library(Rmpi)
}

.Last <- function() {
    if (is.loaded("mpi_initialize")) {
         if (mpi.comm.size()>0) {
             mpi.close.Rslaves()
         }
         .Call("mpi_finalize")
    }
}

mpi.spawn.Rslaves()

#mpi.remote.exec( sprintf("I am worker %d of %d", mpi.comm.rank(), mpi.comm.size()-1))

# Get the slaves to load the example data
file.data <- "/n/airoldifs2/lab/jbischof/reuters_output/mmm_raw_data/parsed_valid_data/feature_word_count_list.RData"
load(file.data)
print(ls())
# Get time
t0 <- proc.time()[3]
mpi.bcast.Robj2slave(feature.count.list)

# Get the desired result
results <- mpi.remote.exec(length(feature.count.list))
print(results)
#sprintf("The variance of the results is %f",var(results))
t1 <- proc.time()[3]
sprintf("Total time elapsed is %f seconds",t1-t0)

# Close down
mpi.close.Rslaves()
mpi.quit()

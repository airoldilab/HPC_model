source("mpi_admin.R")

mpi.admin("open")

# Get the slaves to load data created by master
data.dir <- "/n/airoldifs1/jbischof/reuters_output/mmm_raw_data/parsed_valid_data/"
file.slave.data <- paste(data.dir,"slave_data.Rdata",sep="")
mpi.bcast.Robj2slave(file.slave.data)
mpi.bcast.cmd(load(file.slave.data))

# Send the function to the slaves
mpi.bcast.Robj2slave(theta.slave.fn)

# Call the function in all the slaves to get them ready to
# undertake tasks
mpi.bcast.cmd(theta.slave.fn())

# Get time
t0 <- proc.time()[3]
## results <- mpi.remote.exec(ls())
## print(results)

# Get the desired result
results <- mpi.remote.exec(length(feature.count.list))
print(results)
#sprintf("The variance of the results is %f",var(results))
t1 <- proc.time()[3]
sprintf("Total time elapsed is %f seconds",t1-t0)

mpi.admin("close")

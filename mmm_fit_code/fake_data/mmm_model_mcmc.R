# Master script to organize coordinate ascent algorithm for
# mixed memb hierarchical poisson model

t0 <-  proc.time()[3]

# Load in initialization and fitting functions
funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_fit_code/mmm_fit_functions/"
source(paste(funct.dir,"initialize_params.R",sep=""))
source(paste(funct.dir,"check_conv.R",sep=""))
source(paste(funct.dir,"hpd_gibbs_sampler.R",sep=""))
source(paste(funct.dir,"theta_update.R",sep=""))
source(paste(funct.dir,"tree_update.R",sep=""))
source(paste(funct.dir,"hparam_update.R",sep=""))
source(paste(funct.dir,"hparam_mcmc.R",sep=""))
source(paste(funct.dir,"tree_mcmc.R",sep=""))
source(paste(funct.dir,"xi_update.R",sep=""))
source(paste(funct.dir,"xi_mcmc.R",sep=""))
source(paste(funct.dir,"tau2_hparam_update.R",sep=""))
source(paste(funct.dir,"tau2_hparam_mcmc.R",sep=""))
source(paste(funct.dir,"tree_hparam_mcmc.R",sep=""))
source(paste(funct.dir,"xi_hparam_mcmc.R",sep=""))
source(paste(funct.dir,"indep_chain_metro.R",sep=""))
source(paste(funct.dir,"case_control_samp.R",sep=""))
source("/n/home13/jbischof/reuters_prj/hmc/hmc_functions.R")

# Set up output directory
args <- commandArgs(TRUE)
out.dir <- args[1]
#out.dir <- "/n/airoldifs1/jbischof/reuters_output/mmm_fits/fake_data/"


# Start up MPI; get node rank
source(paste(funct.dir,"mpi_admin.R",sep=""))
source(paste(funct.dir,"global_mpi_functions.R",sep=""))
source(paste(funct.dir,"tree_mpi_functions.R",sep=""))
source(paste(funct.dir,"xi_mpi_functions.R",sep=""))
mpi.start.list <- mpi.admin("open")
mpi.root <- mpi.start.list$mpi.root
mpi.size <- mpi.start.list$mpi.size
mpi.rank <- mpi.start.list$mpi.rank

# Figure out if master or slave
is.master <- mpi.rank == mpi.root

# Set up file to save current/final parameters as updating
file.current.param.list <- paste(out.dir,"current_params.RData",sep="")
file.ave.param.list <- paste(out.dir,"ave_params_gibbs.RData",sep="")
file.final.param.list <- paste(out.dir,"final_params_gibbs.RData",sep="")
# Set up root of file for slave specific data
slave.file.root <- paste(out.dir,"slave_data",sep="")
# Set up timing file
file.time <- paste(out.dir,"compute_time.txt",sep="")

if(is.master){
  # Load in initialized parameters
  outfile.initial <- paste(out.dir,"initialized_train_params.RData",sep="")
  load(outfile.initial)
  
  # Load in job lists
  outfile.joblist <- paste(out.dir,"valid_fit_joblist.RData",sep="")
  load(outfile.joblist)
}


# Load in topic address book
topic.address.book <- read.table("mmm_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)


# If using MPI, get the master and slave nodes working
if(!is.master){mpi.slave.fn(topic.address.book=topic.address.book,
                            file.current.param.list=file.current.param.list,
                            slave.file.root=slave.file.root)
} else {
  final.param.list <- hpd.gibbs.sampler(current.param.list=current.param.list,
                                        topic.address.book=topic.address.book,
                                        ndraws.gibbs=1000,
                                        verbose=FALSE,
                                        print.iter=TRUE,
                                        debug=FALSE,
                                        tree.job.list=tree.job.list,
                                        xi.job.list=xi.job.list,
                                        file.current.param.list=file.current.param.list,
                                        file.ave.param.list=file.ave.param.list,
                                        file.final.param.list=file.final.param.list,
                                        file.time=file.time)
}



# Save output
if(is.master){
  save(final.param.list,file=file.final.param.list)
  t1 <- proc.time()[3]
  cat(sprintf("Finished in %0.2f seconds\n",t1-t0))
}

## # Have master remove all the special data objects created for the slaves
## if(is.master){
##   n.slaves <- mpi.comm.size(0)-1
##   for(slave.id in 1:n.slaves){
##     file.rm <- paste(slave.file.root,slave.id,".RData",sep="")
##     system(paste("rm",file.rm))
##   }
## }

# Close down mpi
mpi.admin("close")



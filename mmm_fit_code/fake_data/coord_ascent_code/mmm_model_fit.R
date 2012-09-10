# Master script to organize coordinate ascent algorithm for
# mixed memb hierarchical poisson model

t0 <-  proc.time()[3]

# Load in initialization and fitting functions
funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_fit_code/mmm_fit_functions/"
source(paste(funct.dir,"initialize_params.R",sep=""))
source(paste(funct.dir,"check_conv.R",sep=""))
source(paste(funct.dir,"global_coor_ascent.R",sep=""))
source(paste(funct.dir,"theta_update.R",sep=""))
source(paste(funct.dir,"tree_update.R",sep=""))
source(paste(funct.dir,"hparam_update.R",sep=""))

# Set up output directory
args <- commandArgs(TRUE)
out.dir <- args[1]
#out.dir <- "/n/airoldifs2/lab/jbischof/reuters_output/mmm_fits/fake_data/"

use.mpi <- TRUE
# If using MPI, start up that process
if(use.mpi){
  # Start up mpi; get node rank
  source(paste(funct.dir,"mpi_admin.R",sep=""))
  source(paste(funct.dir,"global_mpi_functions.R",sep=""))
  source(paste(funct.dir,"tree_mpi_functions.R",sep=""))
  source(paste(funct.dir,"theta_mpi_functions.R",sep=""))
  mpi.start.list <- mpi.admin("open")
  mpi.root <- mpi.start.list$mpi.root
  mpi.size <- mpi.start.list$mpi.size
  mpi.rank <- mpi.start.list$mpi.rank

  # Figure out if master or slave
  is.master <- mpi.rank == mpi.root

  # Set up file to save current parameters as updating
  file.current.param.list <- paste(out.dir,"current_params.RData",sep="")
  # Set up root of file for slave specific data
  slave.file.root <- paste(out.dir,"slave_data",sep="")

  if(is.master){
    # Load in initialized parameters
    outfile.initial <- paste(out.dir,"initialized_train_params.RData",sep="")
    load(outfile.initial)

    # Load in job lists
    outfile.joblist <- paste(out.dir,"valid_fit_joblist.RData",sep="")
    load(outfile.joblist)

    # Set up file to save convergence criteria
    file.post.conv <- paste(out.dir,"joint_post_trace.txt",sep="")
  }
}


# Load in topic address book
topic.address.book <- read.table("mmm_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)


# Fit using true parameters as starting values or crude initializations?
use.true.params <- FALSE
# If not using MPI, load in observed data and initialize parameters
if(!use.mpi){

  # Load in data
  ## load("mmm_feature_count_data.RData")
  ## load("mmm_doc_count_data.RData")
  ## load("mmm_other_data.RData")

  # Load in observed data
  obs.data.dir <- paste(data.dir,data.folder,sep="")
  filename.doc.topic <- paste(obs.data.dir,"doc_topic_list.RData",sep="")
  filename.doc.word.count <- paste(obs.data.dir,"doc_word_count_list.RData",sep="")
  filename.feature.word.count <- paste(obs.data.dir,"feature_word_count_list.RData",sep="")
  filename.doc.length <- paste(obs.data.dir,"doc_length_table.txt",sep="")
  
  data.list <- process.observed.data(filename.doc.topic=filename.doc.topic,
                                     filename.doc.word.count=filename.doc.word.count,
                                     filename.feature.word.count=filename.feature.word.count,
                                     filename.doc.length=filename.doc.length,
                                     L=1)
  
  feature.count.list <- data.list$feature.count.list
  doc.count.list <- data.list$doc.count.list
  doc.length.vec <- data.list$doc.length.vec
  doc.topic.list <- data.list$doc.topic.list

  # Initalize parameters
  # Load in true parameter values
  load("/n/airoldifs2/lab/jbischof/reuters_output/mmm_raw_data/fake_data/mmm_true_params.RData")
  if(use.true.params){current.param.list <- true.param.list}

  # Initialize parameters
  if(!use.true.params){
    current.param.list <-
      initialize.params(feature.count.list=feature.count.list,
                        doc.count.list=doc.count.list,
                        doc.length.vec=doc.length.vec,
                        doc.topic.list=doc.topic.list,
                        corpus.topic="CORPUS",
                        topic.address.book=topic.address.book)

  # For now, borrow hyperparameters from known values
  # Alpha being set to uniform vector to ensure unimodal conditional
  # posterior for theta.d
    alpha.true <- true.param.list$alpha
    alpha.use <- rep(1,length(alpha.true))
    current.param.list[["alpha"]] <- alpha.use
    current.param.list[["nu"]] <- true.param.list$nu
    current.param.list[["sigma2"]] <- true.param.list$sigma2
  }
}


# If using MPI, get the master and slave nodes working
if(use.mpi){
  if(!is.master){mpi.slave.fn(topic.address.book=topic.address.book,
                              file.current.param.list=file.current.param.list,
                              slave.file.root=slave.file.root)
               }
  if(is.master){
    final.param.list <- global.coor.ascent(current.param.list=current.param.list,
                                           #feature.count.list=feature.count.list,
                                           #doc.count.list=doc.count.list,
                                           topic.address.book=topic.address.book,
                                           max.iter=1000,verbose=TRUE,
                                           print.iter=TRUE,print.conv=TRUE,
                                           use.mpi=TRUE,
                                           tree.job.list=tree.job.list,
                                           theta.job.list=theta.job.list,
                                           file.current.param.list=file.current.param.list,
                                           file.post.conv=file.post.conv)
  }
}


# Global coordinate ascent algorithm
if(!use.mpi){
system.time(
final.param.list <- global.coor.ascent(current.param.list=current.param.list,
                                       feature.count.list=feature.count.list,
                                       doc.count.list=doc.count.list,
                                       topic.address.book=topic.address.book,
                                       max.iter=1000,verbose=TRUE,
                                       print.iter=TRUE,print.treeiter=FALSE,
                                       print.thetaiter=FALSE,debug=FALSE,
                                       print.conv=TRUE)
            )
}


# Save output
if(any(!use.mpi,is.master)){
  file.true.out <- paste(out.dir,"final_params_true.RData",sep="")
  file.initialized.out <- paste(out.dir,"final_params_initialized.RData",sep="")
  if(use.true.params){save(final.param.list,file=file.true.out)}
  if(!use.true.params){save(final.param.list,file=file.initialized.out)}
  t1 <- proc.time()[3]
  cat(sprintf("Finished in %f seconds\n",t1-t0))
}

# If using MPI, remove all the special data objects created for the slaves
if(all(use.mpi,is.master)){
  n.slaves <- mpi.comm.size(0)-1
  for(slave.id in 1:n.slaves){
    file.rm <- paste(slave.file.root,slave.id,".RData",sep="")
    system(paste("rm",file.rm))
  }
}

# If using mpi, close down mpi
if(use.mpi){mpi.admin("close")}



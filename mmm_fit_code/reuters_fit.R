# Master script to organize coordinate ascent algorithm for
# mixed memb hierarchical poisson model fit to Reuters training set

# Extract the command line arguments for number of nodes and relevant folders
args <- commandArgs(TRUE)
dir.out <- args[1]

# Load in fitting functions
funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_fit_code/mmm_fit_functions/"
source(paste(funct.dir,"initialize_mmm_params_simple.R",sep=""))
source(paste(funct.dir,"process_obs_data.R",sep=""))
source(paste(funct.dir,"check_conv.R",sep=""))
source(paste(funct.dir,"global_coor_ascent.R",sep=""))
source(paste(funct.dir,"theta_update.R",sep=""))
source(paste(funct.dir,"tree_update.R",sep=""))
source(paste(funct.dir,"hparam_update.R",sep=""))

## # Set up output directory
## main.dir <- "/n/airoldifs2/lab/jbischof/reuters_output/mmm_fits/"
## dir.out <- paste(main.dir,out.folder,sep="")

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
  file.current.param.list <- paste(dir.out,"current_params.RData",sep="")

  # Set up root of file for slave specific data
  slave.file.root <- paste(dir.out,"slave_data",sep="")
  
  if(is.master){
    # Load in initialized parameters
    # If already have results from previous run, start from there
    is.initialized <- length(grep(pattern="current_params.RData",x=dir(dir.out))) > 0
    #is.initialized <- FALSE
    if(is.initialized){load(file.current.param.list)
                       cat("Parameters from old run loaded\n")
    } else {
      # Else load up initialized parameters
      outfile.initial <- paste(dir.out,"initialized_params.RData",sep="")
      load(outfile.initial)
    }

    # Load in job lists
    outfile.joblist <- paste(dir.out,"fit_joblist.RData",sep="")
    load(outfile.joblist)

    # Set up file to save convergence criteria
    file.post.conv <- paste(dir.out,"joint_post_trace.txt",sep="")
  }
}

# Load in data address book
topic.address.book <- read.table("reuters_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)



# If not using MPI, load in observed data and initialize parameters
if(!use.mpi){
  data.dir <- "/n/airoldifs2/lab/jbischof/reuters_output/mmm_raw_data/"
  obs.data.dir <- "/n/airoldifs2/lab/jbischof/reuters_output/mmm_raw_data/parsed_train_data500/"
  filename.doc.topic <- paste(obs.data.dir,"doc_topic_list.RData",sep="")
  filename.doc.word.count <- paste(obs.data.dir,"doc_word_count_list.RData",sep="")
  filename.feature.word.count <- paste(obs.data.dir,"feature_word_count_list.RData",sep="")
  filename.doc.length <- paste(obs.data.dir,"doc_length_table.txt",sep="")
  
  data.list <- process.observed.data(filename.doc.topic=filename.doc.topic,
                                     filename.doc.word.count=filename.doc.word.count,
                                     filename.feature.word.count=filename.feature.word.count,
                                     filename.doc.length=filename.doc.length,
                                     L=140)
  
  feature.count.list <- data.list$feature.count.list
  doc.count.list <- data.list$doc.count.list
  doc.length.vec <- data.list$doc.length.vec
  doc.topic.list <- data.list$doc.topic.list

  
  # Load in initialized parameters

  # See if already have copy of initialized params
  outfile.initial <- paste(dir.out,"initialized_params.RData",sep="")
  is.initialized <- length(grep(pattern=outfile.initial,x=dir(dir.out))) > 0

  if(is.initialized){load(outfile.initial)}
  if(!is.initialized){
    filename.mu.corpus <- paste(data.dir,"initialized_corpus_mu.txt",sep="")
    filename.mu.param.vecs <- paste(data.dir,"initialized_mu.txt",sep="")
    filename.tau2.param.vecs <- paste(data.dir,"initialized_tau2.txt",sep="")
    filename.theta.param.vecs <- paste(obs.data.dir,"initialized_theta.txt",sep="")
    
    current.param.list <- initialize.mmm.params(filename.mu.corpus=filename.mu.corpus,
                                                filename.mu.param.vecs=filename.mu.param.vecs,
                                                filename.tau2.param.vecs=filename.tau2.param.vecs,
                                                filename.theta.param.vecs=filename.theta.param.vecs,
                                                nu=60,sigma2=0.01,
                                                feature.count.list=feature.count.list,
                                                doc.count.list=doc.count.list,
                                                doc.length.vec=doc.length.vec,
                                                doc.topic.list=doc.topic.list,
                                                corpus.topic="CORPUS",
                                                topic.address.book=topic.address.book)
    
    outfile.initial <- paste(dir.out,"initialized_params.RData",sep="")
    save(current.param.list,file=outfile.initial)
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
  outfile <- paste(dir.out,"final_params.RData",sep="")
  save(final.param.list,file=outfile)
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


## # Debugging tree optimization
## word.id <- names(feature.count.list)[1]
## system.time(
## out.optim.tree <- optim.tree(job.id=word.id,current.param.list=current.param.list,
##                              doc.length.vec=doc.length.vec,
##                              doc.topic.list=doc.topic.list,count.list=feature.count.list,
##                              topic.address.book=topic.address.book)
##             )

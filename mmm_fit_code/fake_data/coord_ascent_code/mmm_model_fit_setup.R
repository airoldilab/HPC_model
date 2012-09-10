# Script to ready data for MPI implementation of global coordinate ascent

# Load in fitting functions
#out.dir <- "/n/airoldifs2/lab/jbischof/reuters_output/mmm_fits/fake_data/"
funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_fit_code/mmm_fit_functions/"
source(paste(funct.dir,"initialize_params.R",sep=""))
source(paste(funct.dir,"initialize_mmm_params_simple.R",sep=""))
source(paste(funct.dir,"process_obs_data.R",sep=""))
source(paste(funct.dir,"check_conv.R",sep=""))

# Extract the command line arguments for number of nodes
args <- commandArgs(TRUE)
print(args)
n.nodes <- as.numeric(args[1])
n.slaves <- n.nodes-1
obs.data.dir <- args[2]
out.dir <- args[3]

# Load in data address book
topic.address.book <- read.table("mmm_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)

# Load in observed data
filename.doc.topic <- paste(obs.data.dir,"doc_topic_list.RData",sep="")
filename.doc.word.count <- paste(obs.data.dir,"doc_word_count_list.RData",sep="")
filename.feature.word.count <- paste(obs.data.dir,"feature_word_count_list.RData",sep="")
filename.doc.length <- paste(obs.data.dir,"doc_length_table.txt",sep="")

data.list <- process.observed.data(filename.doc.topic=filename.doc.topic,
                                   filename.doc.word.count=filename.doc.word.count,
                                   filename.feature.word.count=filename.feature.word.count,
                                   filename.doc.length=filename.doc.length,L=1)

feature.count.list <- data.list$feature.count.list
doc.count.list <- data.list$doc.count.list
doc.length.vec <- data.list$doc.length.vec
doc.topic.list <- data.list$doc.topic.list


# Initalize parameters
# Fit using true parameters as starting values or crude initializations?
use.true.params <- FALSE
# Load in true parameter values
load("/n/airoldifs2/lab/jbischof/reuters_output/mmm_raw_data/fake_data/mmm_true_params.RData")
if(use.true.params){current.param.list <- true.param.list}

# Initialize parameters
if(!use.true.params){
  filename.doc.xi <- paste(obs.data.dir,"doc_xi_list.RData",sep="")
  filename.theta.param.vecs <- paste(obs.data.dir,"initialized_theta.txt",sep="")
  filename.eta.vec <- paste(obs.data.dir,"eta_vec.txt",sep="")
  current.param.list <-
    initialize.params(feature.count.list=feature.count.list,
                      doc.count.list=doc.count.list,
                      doc.length.vec=doc.length.vec,
                      doc.topic.list=doc.topic.list,
                      #filename.doc.xi=filename.doc.xi,
                      filename.eta.vec=filename.eta.vec,
                      filename.theta.param.vecs=filename.theta.param.vecs,
                      corpus.topic="CORPUS",
                      topic.address.book=topic.address.book)
  
  # For now, borrow hyperparameters from known values
  ## current.param.list[["nu"]] <- true.param.list$nu
  ## current.param.list[["sigma2"]] <- true.param.list$sigma2
}

outfile.initial <- paste(out.dir,"initialized_train_params.RData",sep="")
save(current.param.list,file=outfile.initial)


# Create job lists and output data for slaves
# Set up root of file for slave specific data
slave.file.root <- paste(out.dir,"slave_data",sep="")

out.job.lists <- get.job.lists.and.data(feature.count.list.orig=feature.count.list,
                                        doc.count.list.orig=doc.count.list,
                                        doc.topic.list.orig=doc.topic.list,
                                        doc.length.vec=doc.length.vec,
                                        theta.param.vecs=current.param.list$theta.param.vecs,
                                        n.slaves=n.slaves,
                                        slave.file.root=slave.file.root,
                                        verbose=TRUE)
tree.job.list <- out.job.lists$tree.job.list
theta.job.list <- out.job.lists$theta.job.list

outfile.joblist <- paste(out.dir,"valid_fit_joblist.RData",sep="")
save(tree.job.list,theta.job.list,file=outfile.joblist)

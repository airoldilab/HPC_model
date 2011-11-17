# Script to ready data for MPI implementation of global coordinate ascent


# Extract the command line arguments for number of nodes and relevant folders
args <- commandArgs(TRUE)
print(args)
n.nodes <- as.numeric(args[1])
n.slaves <- n.nodes-1
dir.out <- args[2]
data.dir <- args[3]
data.folder <- args[4]
cutoff <- args[5]
obs.data.dir <- paste(data.dir,data.folder,sep="")

# Load in fitting functions
funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_fit_code/mmm_fit_functions/"
source(paste(funct.dir,"initialize_params.R",sep=""))
source(paste(funct.dir,"process_obs_data.R",sep=""))
source(paste(funct.dir,"tau2_hparam_update.R",sep=""))
source(paste(funct.dir,"tau2_hparam_mcmc.R",sep=""))
source(paste(funct.dir,"check_conv.R",sep=""))

# Load in data address book
topic.address.book <- read.table("reuters_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)

# Set up root of file for slave specific data
slave.file.root <- paste(dir.out,"slave_data",sep="")

# Load in observed data
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


# Initialization files
#filename.doc.xi <- paste(obs.data.dir,"doc_xi_list.RData",sep="")
filename.eta.vec <- paste(obs.data.dir,"eta_vec.txt",sep="")
filename.theta.param.vecs <- paste(obs.data.dir,"initialized_theta_sparse.RData",sep="")
filename.mu.param.vecs <- paste(obs.data.dir,"initialized_mu",cutoff,".txt",sep="")
filename.mu.corpus.vec <- paste(obs.data.dir,"initialized_corpus_mu",cutoff,".txt",sep="")
filename.tau2.vec <- paste(obs.data.dir,"initialized_tau2",cutoff,".txt",sep="")

# Set up current.param.list from initialized parameters
current.param.list <-
  initialize.params(feature.count.list=feature.count.list,
                    doc.count.list=doc.count.list,
                    doc.length.vec=doc.length.vec,
                    doc.topic.list=doc.topic.list,
                    filename.mu.param.vecs=filename.mu.param.vecs,
                    filename.mu.corpus.vec=filename.mu.corpus.vec,
                    filename.tau2.vec=filename.tau2.vec,
                    filename.eta.vec=filename.eta.vec,
                    filename.theta.param.vecs=filename.theta.param.vecs,
                    filename.doc.xi=NULL,
                    corpus.topic="CORPUS",
                    topic.address.book=topic.address.book,
                    lambda2.start=35,
                    eta.offset=-5,
                    kappa.0=-1,
                    omega2.0=0,
                    full.Sigma=FALSE)

outfile.initial <- paste(dir.out,"initialized_params.RData",sep="")
save(current.param.list,file=outfile.initial)


# Create job lists and output data for slaves
out.job.lists <- get.job.lists.and.data(feature.count.list.orig=feature.count.list,
                                        doc.count.list.orig=doc.count.list,
                                        doc.topic.list.orig=doc.topic.list,
                                        doc.length.vec=doc.length.vec,
                                        theta.param.vecs=current.param.list$theta.param.vecs,
                                        mu.corpus.vec=current.param.list$mu.corpus.vec,
                                        n.slaves=n.slaves,
                                        slave.file.root=slave.file.root,
                                        verbose=TRUE)
tree.job.list <- out.job.lists$tree.job.list
xi.job.list <- out.job.lists$xi.job.list

outfile.joblist <- paste(dir.out,"fit_joblist.RData",sep="")
save(tree.job.list,xi.job.list,file=outfile.joblist)

# Script to ready data for MPI implementation of global coordinate ascent


# Extract the command line arguments for number of nodes and relevant folders
args <- commandArgs(TRUE)
print(args)
n.nodes <- as.numeric(args[1])
n.slaves <- n.nodes
out.dir <- args[2]
data.dir <- args[3]
data.folder <- args[4]
fit.dir <- args[5]
cutoff <- args[6]
obs.data.dir <- paste(data.dir,data.folder,sep="")

# Load in fitting functions
fit.funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_fit_code/mmm_fit_functions/"
source(paste(fit.funct.dir,"process_obs_data.R",sep=""))
source(paste(fit.funct.dir,"initialize_params.R",sep=""))
source(paste(fit.funct.dir,"xi_update.R",sep=""))
class.funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_class_code/mmm_class_functions/"
source(paste(class.funct.dir,"classify_new_docs_setup.R",sep=""))
source(paste(class.funct.dir,"delta_crossvalid.R",sep=""))

# Load in data address book
topic.address.book <- read.table("reuters_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)

# Set up root of file for slave specific data
slave.file.root <- paste(out.dir,"slave_data",sep="")

# Load in observed data
filename.doc.topic <- paste(obs.data.dir,"doc_topic_list.RData",sep="")
filename.doc.word.count <- paste(obs.data.dir,"doc_word_count_list.RData",sep="")
filename.doc.length <- paste(obs.data.dir,"doc_length_table.txt",sep="")

data.list <- process.observed.data(filename.doc.topic=filename.doc.topic,
                                   filename.doc.word.count=filename.doc.word.count,
                                   filename.doc.length=filename.doc.length,
                                   L=140,classify=TRUE)

doc.count.list <- data.list$doc.count.list
doc.length.vec <- data.list$doc.length.vec
doc.topic.list <- data.list$doc.topic.list


# Initialization files
# Note that ave.param.list missing a lot of information in current.param.list
filename.ave.params <- paste(fit.dir,"ave_params_gibbs_class.RData",sep="")
load(filename.ave.params)
current.param.list <- gen.class.data(ave.param.list=ave.param.list,
                                     doc.ids=names(doc.count.list),
                                     topic.address.book=topic.address.book)

# Output current.param.list to file
outfile.current.param.list <- paste(out.dir,"current_param_list.RData",sep="")
save(current.param.list,file=outfile.current.param.list)

# Will have to remove words from doc.count.list if not seen in training set
doc.count.list <- remove.new.words(doc.count.list=doc.count.list,
                                   words.train=rownames(current.param.list$mu.param.vecs))


# Create job lists and output data for slaves
out.job.lists <- get.job.lists.and.data(doc.count.list.orig=doc.count.list,
                                        doc.topic.list.orig=doc.topic.list,
                                        doc.length.vec=doc.length.vec,
                                        theta.param.vecs=current.param.list$theta.param.vecs,
                                        mu.corpus.vec=current.param.list$mu.corpus.vec,
                                        n.slaves=n.slaves,slave.file.root=slave.file.root,
                                        verbose=TRUE,classify=TRUE)

xi.job.list <- out.job.lists$xi.job.list

outfile.joblist <- paste(out.dir,"class_joblist.RData",sep="")
save(xi.job.list,file=outfile.joblist)

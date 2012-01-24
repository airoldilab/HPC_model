# Script to load in expected rates and find top words for every topic

args <- commandArgs(TRUE)
fit.dir <- args[1]
analysis.dir <- args[2]
file.ave.params <- args[3]
file.kept.word.stems <- args[4]
topic.get <- args[5]

# Load in sorting function
funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_analysis_code/mmm_analysis_functions/"
source(paste(funct.dir,"get_top_items.R",sep=""))

# Get kept word ids
load(file.ave.params)
word.stem.table <- read.table(file.kept.word.stems,header=FALSE,
                              row.names=1)
kept.word.ids <- rownames(word.stem.table)

# Extract mu and tau2 parameters for kept words
mu.param.vecs <- ave.param.list$mu.param.vecs[kept.word.ids,]
mu.word.ids <- rownames(mu.param.vecs)
log.tau2.param.vecs <- log(ave.param.list$tau2.param.vecs[kept.word.ids,])
tau2.word.ids <- rownames(tau2.param.vecs)


# 


file.out <- paste(analysis.dir,"top_topic_mu.txt",sep="")
write.table(top.mu,file.out,quote=FALSE,sep=" ",col.names=FALSE)

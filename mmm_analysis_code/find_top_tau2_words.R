# Script to load in expected phis and find top words for every topic

args <- commandArgs(TRUE)
fit.dir <- args[1]
analysis.dir <- args[2]
file.ave.params <- args[3]
file.kept.word.stems <- args[4]
n.get <- args[5]

# Load in sorting function
funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_analysis_code/mmm_analysis_functions/"
source(paste(funct.dir,"get_top_items.R",sep=""))

load(file.ave.params)
word.stem.table <- read.table(file.kept.word.stems,header=FALSE,
                              row.names=1)
kept.word.ids <- rownames(word.stem.table)
tau2.param.vecs <- ave.param.list$tau2.param.vecs[kept.word.ids,]
tau2.word.ids <- rownames(tau2.param.vecs)

top.tau2 <- t(apply(tau2.param.vecs,2,get.top.items,word.id.vec=tau2.word.ids,
                   word.stem.table=word.stem.table,n.get=n.get))
file.out <- paste(analysis.dir,"top_topic_tau2.txt",sep="")
write.table(top.tau2,file.out,quote=FALSE,sep=" ",col.names=FALSE)

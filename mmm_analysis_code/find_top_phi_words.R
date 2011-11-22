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
fit.funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_fit_code/mmm_fit_functions/"
source(paste(fit.funct.dir,"hpd_gibbs_sampler.R",sep=""))

load(file.ave.params)

word.stem.table <- read.table(file.kept.word.stems,header=FALSE,
                              row.names=1)
kept.word.ids <- rownames(word.stem.table)
phi.param.vecs <- ave.param.list$phi.param.vecs[kept.word.ids,]
phi.word.ids <- rownames(phi.param.vecs)

mu.param.vecs <- ave.param.list$mu.param.vecs[kept.word.ids,]
mu.corpus.vec <- ave.param.list$mu.corpus.vec[kept.word.ids]
parent.child.list <- ave.param.list$parent.child.list 
phi.parent.param.vecs <- get.phi.parent.vec(mu.param.vecs,mu.corpus.vec,
                                            parent.child.list)
#print(head(phi.parent.param.vecs))
phi.ave.param.vecs <- get.phi.ave.vec(phi.param.vecs,phi.parent.param.vecs,
                                      parent.child.list)
#print(head(phi.ave.param.vecs))

top.phi <- t(apply(phi.param.vecs,2,get.top.items,word.id.vec=phi.word.ids,
                   word.stem.table=word.stem.table,n.get=n.get))
file.out <- paste(analysis.dir,"top_topic_phi.txt",sep="")
write.table(top.phi,file.out,quote=FALSE,sep=" ",col.names=FALSE)

top.parent.phi <- t(apply(phi.parent.param.vecs,2,
                          get.top.items,word.id.vec=phi.word.ids,
                          word.stem.table=word.stem.table,n.get=n.get))
file.out <- paste(analysis.dir,"top_parent_phi.txt",sep="")
write.table(top.parent.phi,file.out,quote=FALSE,sep=" ",col.names=FALSE)

top.ave.phi <- t(apply(phi.ave.param.vecs,2,get.top.items,
                       word.id.vec=phi.word.ids,
                       word.stem.table=word.stem.table,n.get=n.get))
file.out <- paste(analysis.dir,"top_ave_phi.txt",sep="")
write.table(top.ave.phi,file.out,quote=FALSE,sep=" ",col.names=FALSE)

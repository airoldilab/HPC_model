# Script to get relevant comparisons of most common and
# most discriminative words

args <- commandArgs(TRUE)
analysis.dir <- args[1]
n.use <- args[2]

library("xtable")

file.phi <- paste(analysis.dir,"top_topic_phi.txt",sep="")
topic.phi <- as.matrix(read.table(file.phi,row.names=1,header=FALSE))
colnames(topic.phi) <- NULL
file.parent.phi <- paste(analysis.dir,"top_parent_phi.txt",sep="")
topic.parent.phi <- as.matrix(read.table(file.parent.phi,row.names=1,header=FALSE))
colnames(topic.parent.phi) <- NULL
file.ave.phi <- paste(analysis.dir,"top_ave_phi.txt",sep="")
file.mu <- paste(analysis.dir,"top_topic_mu.txt",sep="")
topic.mu <- as.matrix(read.table(file.mu,row.names=1,header=FALSE))
colnames(topic.mu) <- NULL

comp.lists <- function(mat1,mat2,topic.comp,n.use,ncol=1){
  n.row <- trunc(n.use/ncol)
  words1 <- mat1[topic.comp,]
  words2 <- mat2[topic.comp,]
  mat1 <- matrix(words1[1:n.use],nrow=n.row,ncol=ncol,byrow=FALSE)
  mat2 <- matrix(words2[1:n.use],nrow=n.row,ncol=ncol,byrow=FALSE)
  mat.comp <- cbind(mat1,mat2)
  dimnames(mat.comp) <- NULL
  print(topic.comp)
  xtable(mat.comp,align="lc|c")
}

topic.comp <- "M142"
comp.lists(topic.phi,topic.mu,topic.comp,n.use)

topic.comp <- "GENV"
comp.lists(topic.phi,topic.mu,topic.comp,n.use)

topic.comp <- "C331"
comp.lists(topic.parent.phi,topic.mu,topic.comp,n.use)

topic.comp <- "GCRIM"
comp.lists(topic.phi,topic.mu,topic.comp,n.use)

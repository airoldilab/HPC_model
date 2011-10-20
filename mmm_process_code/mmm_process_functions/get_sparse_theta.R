# Script to turn initialized theta table into sparse matrix

library(Matrix)

args <- commandArgs(TRUE)
data.dir <- args[1]

filename.theta.param.vecs <- paste(data.dir,"initialized_theta.txt",sep="")
outfile <- paste(data.dir,"initialized_theta_sparse.RData",sep="")

theta.param.vecs <- read.table(filename.theta.param.vecs,as.is=TRUE)
theta.param.vecs <- as(as.matrix(theta.param.vecs), "sparseMatrix")
save(theta.param.vecs,file=outfile)

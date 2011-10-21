# Script to average over observation level latent variables to assess
# quality of draws

## args <- commandArgs(TRUE)
## out.dir <- args[1]
out.dir <- "/n/airoldifs1/jbischof/reuters_output/mmm_fits/fake_data/"
data.out.dir <- paste(out.dir,"slave_data/",sep="")
file.final.param.list <- paste(data.out.dir,"final_params_gibbs.RData",sep="")
truth.dir <- "/n/airoldifs1/jbischof/reuters_output/mmm_raw_data/fake_data/"
load(paste(truth.dir,"mmm_true_params.RData",sep=""))
burnin <- 600

# Load in final.param.list
load(file.final.param.list)

# Get rid of NAs
final.param.list <- lapply(final.param.list,na.omit)
# Figure out where NAs are
na.pos <- attr(final.param.list$psi,which="na.action")

# Create some traceplots of HMC draws
library("MCMCpack")
pdf(paste(out.dir,"HMC_traceplots.pdf",sep=""),width=8,height=6)
par(mfrow=c(2,2))
plot(na.omit(final.param.list$mu.param.vecs[1,1,]),ylab="mu_1,1",xlab="Iteration",
main="Traceplot of HMC draws of first mu",type="l")
#abline(h=true.param.list$mu.param.vecs[1,1],col="red")
plot(na.omit(final.param.list$xi.param.vecs[1,1,]),ylab="xi_1,1",xlab="Iteration",
main="Traceplot of HMC draws of first xi",type="l")
#abline(h=true.param.list$mu.param.vecs[1,1],col="red")
acf(na.omit(final.param.list$mu.param.vecs[1,1,]),main="ACF plot for first mu")
acf(na.omit(final.param.list$xi.param.vecs[1,1,]),main="ACF plot for first xi")
dev.off()

# Average over individual level params
final.param.list$mu.param.vecs <-
  apply(final.param.list$mu.param.vecs[,,-c(1:burnin,na.pos)],1:2,mean)
final.param.list$tau2.param.vecs <-
  apply(final.param.list$tau2.param.vecs[,,-c(1:burnin,na.pos)],1:2,mean)
final.param.list$xi.param.vecs <-
  apply(final.param.list$xi.param.vecs[,,-c(1:burnin,na.pos)],1:2,mean)
final.param.list$eta.vec <-
  apply(final.param.list$eta.vec[-c(1:burnin,na.pos),],1:2,mean)
final.param.list$mu.corpus.vec <-
  apply(final.param.list$mu.corpus.vec[-c(1:burnin,na.pos),],2,mean)

# Save averaged parameter values
file.out <- paste(data.out.dir,"final_ave_params_gibbs.RData",sep="")
save(final.param.list,file=file.out)

## plot(na.omit(final.param.list$mu.corpus.vec[,1]),ylab="mu_1,1",xlab="Iteration",
## main="Traceplot of HMC draws of first mu",type="l")
## abline(h=final.param.list$mu.corpus.vec[1],col="red")


#####################################################################
####################################################################
# Code graveyard ##################################################
##################################################################
#################################################################

## # Get rid of NAs everywhere else
## final.param.list$psi <- na.omit(final.param.list$psi)
## final.param.list$gamma <- na.omit(final.param.list$gamma)
## final.param.list$nu <- na.omit(final.param.list$nu)
## final.param.list$sigma2 <- na.omit(final.param.list$sigma2)
## final.param.list$lambda2 <- na.omit(final.param.list$lambda2)

# Script to get traceplots for HMC draws

fit.dir <- "/n/home13/jbischof/jbischof/reuters_output/mmm_fits/fit_train500/"
out.dir <- "/n/home13/jbischof/jbischof/reuters_output/mmm_analysis_out/"
file.final.params <- paste(fit.dir,"final_params_gibbs.RData",sep="")

load(file.final.params)

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

# Load in fitted parameters and truth
dir.out <- "/n/airoldifs1/jbischof/reuters_output/mmm_fits/fake_data/slave_data/"
load(paste(dir.out,"final_ave_params_gibbs.RData",sep=""))
truth.dir <- "/n/airoldifs1/jbischof/reuters_output/mmm_raw_data/fake_data/"
load(paste(truth.dir,"mmm_true_params.RData",sep=""))

burnin <- 200

# Which words fit in model?
features.use <- rownames(final.param.list$mu.param.vecs)

# Which docs fit in model?
docs.use <- rownames(final.param.list$xi.param.vecs)

# Compare rates
mu.f.fit <- final.param.list$mu.param.vecs
mu.f.true <- true.param.list$mu.param.vecs[features.use,]

# Compare corpus level rates
mu.0.fit <- final.param.list$mu.corpus.vec
mu.0.true <- true.param.list$mu.corpus.vec[features.use]

# Compare discrimination parameters
tau2f.fit <- final.param.list$tau2.param.vecs
tau2f.true <- true.param.list$tau2.param.vecs[features.use,]

# Compare membership parameters
xi.d.fit <- final.param.list$xi.param.vecs
xi.d.true <- true.param.list$xi.param.vecs[docs.use,]

# Compare topic log odds mean parameters
eta.vec.fit <- final.param.list$eta.vec
eta.vec.true <- true.param.list$eta.vec

# Compare hyperparameters
psi.fit <- final.param.list$psi
psi.true <- true.param.list$psi
gamma2.fit <- final.param.list$gamma^2
gamma2.true <- true.param.list$gamma^2
nu.fit <- final.param.list$nu
nu.true <- true.param.list$nu
sigma2.fit <- final.param.list$sigma2
sigma2.true <- true.param.list$sigma2
lambda2.fit <- final.param.list$lambda2
lambda2.true <- true.param.list$lambda2

# Create pdf of difference histograms
file.pdf <- paste(dir.out,"fit_true_param_diff.pdf",sep="")
pdf(file.pdf,width=8,height=6)
par(mfrow=c(2,2))
hist(mu.f.fit-mu.f.true,'fd',main="Fitted versus true mu.f difference",xlab="mu.f")
abline(v=0,col="red")
hist(mu.0.fit-mu.0.true,'fd',main="Fitted versus true mu.f.0 difference",xlab="mu.f.0")
abline(v=0,col="red")
hist(tau2f.fit-tau2f.true,'fd',main="Fitted versus true tau2 difference",xlab="tau2")
abline(v=0,col="red")
hist(xi.d.fit-xi.d.true,'fd',main="Fitted versus true xi.d difference",xlab="xi.d")
abline(v=0,col="red")
dev.off()

file.hparam.pdf <- paste(dir.out,"fit_tree_hparam_hist.pdf",sep="")
pdf(file.hparam.pdf,width=8,height=6)
par(mfrow=c(2,2))
hist(psi.fit[-c(1:burnin)],'fd',main="Samples from psi posterior",xlab="psi draws")
abline(v=psi.true,col="red")
hist(gamma2.fit[-c(1:burnin)],'fd',main="Samples from gamma2 posterior",xlab="gamma2 draws")
abline(v=gamma2.true,col="red")
hist(nu.fit[-c(1:burnin)],'fd',main="Samples from nu posterior",xlab="nu draws")
abline(v=nu.true,col="red")
hist(sigma2.fit[-c(1:burnin)],'fd',main="Samples from sigma2 posterior",xlab="sigma2 draws")
abline(v=sigma2.true,col="red")
dev.off()


file.hparam.trace.pdf <- paste(dir.out,"fit_true_hparam_trace.pdf",sep="")
pdf(file.hparam.trace.pdf,width=8,height=6)
par(mfrow=c(2,2))
plot(psi.fit,main="Traceplot of psi draws",xlab="Iteration",ylab="psi",type="l")
abline(h=psi.true,col="red")
plot(gamma2.fit,main="Traceplot of gamma2 draws",xlab="Iteration",ylab="gamma2",type="l")
abline(h=gamma2.true,col="red")
plot(nu.fit,main="Traceplot of from nu draws",xlab="Iteration",ylab="nu",type="l")
abline(h=nu.true,col="red")
plot(sigma2.fit,main="Traceplot of sigma2 draws",xlab="Iteration",ylab="sigma2",type="l")
abline(h=sigma2.true,col="red")
dev.off()

file.hparam.trace2.pdf <- paste(dir.out,"fit_true_hparam_trace2.pdf",sep="")
pdf(file.hparam.trace2.pdf,width=8,height=6)
par(mfrow=c(2,2))
plot(eta.vec.fit[,1],main="Traceplot of eta1 draws",xlab="Iteration",ylab="eta1",type="l")
abline(h=eta.vec.true[1],col="red")
plot(eta.vec.fit[,2],main="Traceplot of eta2 draws",xlab="Iteration",ylab="eta2",type="l")
abline(h=eta.vec.true[2],col="red")
plot(eta.vec.fit[,3],main="Traceplot of eta3 draws",xlab="Iteration",ylab="eta3",type="l")
abline(h=eta.vec.true[3],col="red")
plot(lambda2.fit,main="Traceplot of lambda2 draws",xlab="Iteration",ylab="lambda2",type="l")
abline(h=lambda2.true,col="red")
dev.off()

burnin <- 400
file.hparam.pdf <- paste(dir.out,"fit_doc_hparam_hist.pdf",sep="")
pdf(file.hparam.pdf,width=8,height=6)
par(mfrow=c(2,2))
hist(eta.vec.fit[-c(1:burnin),1],main="Samples from eta1 posterior",xlab="eta1")
abline(v=eta.vec.true[1],col="red")
hist(eta.vec.fit[-c(1:burnin),2],main="Samples from eta2 posterior",xlab="eta2")
abline(v=eta.vec.true[2],col="red")
hist(eta.vec.fit[-c(1:burnin),3],main="Samples from eta3 posterior",xlab="eta3")
abline(v=eta.vec.true[3],col="red")
hist(lambda2.fit[-c(1:burnin)],main="Samples from lambda2 posterior",xlab="lambda2")
abline(v=lambda2.true,col="red")
dev.off()

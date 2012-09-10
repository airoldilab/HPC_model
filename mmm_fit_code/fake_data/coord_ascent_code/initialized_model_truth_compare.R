# Load in fitted parameters and truth
dir.out <- "/n/airoldifs2/lab/jbischof/reuters_output/mmm_fits/fake_data_debug/"
load(paste(dir.out,"final.params.initialized.RData",sep=""))
load("mmm_true_params.RData")

# Which words fit in model?
features.use <- rownames(final.param.list$mu.param.vecs)

# Which docs fit in model?
docs.use <- rownames(final.param.list$theta.param.vecs)

# Compare rates
mu.f.fit <- final.param.list$mu.param.vecs
mu.f.true <- true.param.list$mu.param.vecs[features.use,]

# Compare corpus level rates
mu.0.fit <- final.param.list$mu.corpus.vec
mu.0.true <- true.param.list$mu.corpus.vec[features.use]

# Compare discrimination parameters
tau2f.fit <- final.param.list$tau2.param.vecs[,1:ncol(true.param.list$tau2.param.vecs)]
tau2f.true <- true.param.list$tau2.param.vecs[features.use,]
diff.tau2 <- tau2f.fit-tau2f.true

# Compare membership parameters
theta.d.fit <- final.param.list$theta.param.vecs[final.param.list$theta.param.vecs > 0]
theta.d.true <- true.param.list$theta.param.vecs[docs.use,][true.param.list$theta.param.vecs > 0]

# Create pdf of difference histograms
file.pdf <- paste(dir.out,"fit_true_param_diff.pdf",sep="")
pdf(file.pdf,width=12,height=12)
par(mfrow=c(2,2))
hist(mu.f.fit-mu.f.true,'fd',main="Fitted versus true mu.f difference",xlab="mu.f")
hist(mu.0.fit-mu.0.true,'fd',main="Fitted versus true mu.f.0 difference",xlab="mu.f.0")
hist(diff.tau2[diff.tau2>-2],'fd',main="Fitted versus true tau2 difference",xlab="tau2")
hist(theta.d.fit-theta.d.true,'fd',main="Fitted versus true theta.d difference",xlab="theta.d")
dev.off()

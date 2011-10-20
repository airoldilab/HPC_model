# Script to experiment with MCMC inference for
# mixed memb hierarchical poisson model

# Load in initialization and fitting functions
funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_fit_code/mmm_fit_functions/"
source(paste(funct.dir,"initialize_params.R",sep=""))
source(paste(funct.dir,"initialize_mmm_params_simple.R",sep=""))
source(paste(funct.dir,"process_obs_data.R",sep=""))
source(paste(funct.dir,"check_conv.R",sep=""))
source(paste(funct.dir,"global_coor_ascent.R",sep=""))
source(paste(funct.dir,"theta_update.R",sep=""))
source(paste(funct.dir,"tree_update.R",sep=""))
source(paste(funct.dir,"hparam_update.R",sep=""))
source(paste(funct.dir,"tree_mcmc.R",sep=""))
source(paste(funct.dir,"xi_update.R",sep=""))
source(paste(funct.dir,"xi_mcmc.R",sep=""))
source(paste(funct.dir,"tau2_hparam_update.R",sep=""))
source(paste(funct.dir,"tau2_hparam_mcmc.R",sep=""))
source(paste(funct.dir,"indep_chain_metro.R",sep=""))
source("/n/home13/jbischof/reuters_prj/hmc/hmc_functions.R")

# Set up output directory
## args <- commandArgs(TRUE)
## out.dir <- args[1]
main.dir <- "/n/airoldifs1/jbischof/reuters_output/"
out.dir <- paste(main.dir,"mmm_fits/fake_data/",sep="")
obs.data.dir <- paste(main.dir,"mmm_raw_data/fake_data/parsed_train_data/",sep="")
out.dir <- paste(main.dir,"mmm_fits/fake_data/slave_data/",sep="")

# Load in topic address book
topic.address.book <- read.table("mmm_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)


# Load in observed data
filename.doc.topic <- paste(obs.data.dir,"doc_topic_list.RData",sep="")
filename.doc.word.count <- paste(obs.data.dir,"doc_word_count_list.RData",sep="")
filename.feature.word.count <- paste(obs.data.dir,"feature_word_count_list.RData",sep="")
filename.doc.length <- paste(obs.data.dir,"doc_length_table.txt",sep="")

data.list <- process.observed.data(filename.doc.topic=filename.doc.topic,
                                   filename.doc.word.count=filename.doc.word.count,
                                   filename.feature.word.count=filename.feature.word.count,
                                   filename.doc.length=filename.doc.length,
                                   L=1)

feature.count.list <- data.list$feature.count.list
doc.count.list <- data.list$doc.count.list
doc.length.vec <- data.list$doc.length.vec
doc.topic.list <- data.list$doc.topic.list

# Initalize parameters
# Fit using true parameters as starting values or crude initializations?
use.true.params <- TRUE
load("/n/airoldifs1/jbischof/reuters_output/mmm_raw_data/fake_data/mmm_true_params.RData")
if(use.true.params){
  current.param.list <- true.param.list
}

# Initialize parameters
if(!use.true.params){
  filename.doc.xi <- paste(obs.data.dir,"doc_xi_list.RData",sep="")
  current.param.list <-
    initialize.params(feature.count.list=feature.count.list,
                      doc.count.list=doc.count.list,
                      doc.length.vec=doc.length.vec,
                      doc.topic.list=doc.topic.list,
                      filename.doc.xi=filename.doc.xi,
                      corpus.topic="CORPUS",
                      topic.address.book=topic.address.book)
  
  # For now, borrow hyperparameters from known values
  # Alpha being set to uniform vector to ensure unimodal conditional
  # posterior for theta.d
  alpha.true <- true.param.list$alpha
  alpha.use <- rep(1,length(alpha.true))
  current.param.list[["alpha"]] <- alpha.use
  current.param.list[["nu"]] <- true.param.list$nu
  current.param.list[["sigma2"]] <- true.param.list$sigma2
}

# Check optimizer
tau2.vec <- as.vector(current.param.list$tau2.param.vecs)
optim.out <- joint.optim.gamma(tau2.vec,hessian=TRUE)
lambda.fit <- optim.out$lambda
kappa.fit <- optim.out$kappa
profile.optim.gamma(tau2.vec)

prop.inv.cov <- -hessian.gamma(tau2.vec=tau2.vec,lambda=lambda.fit,
                                 kappa=kappa.fit)

convert.hparams(c(kappa.fit,lambda.fit))

true.param.list$nu
true.param.list$sigma2

# Check importance sampler
gamma.import.out <- gamma.import.sampler(current.param.list=current.param.list,
                                         ndraws=10000,prop.scale=1)

head(gamma.import.out$log.weights)
hist(gamma.import.out$log.weights,'fd')
abline(v=1,col="red")


# Run independence chain metropolis sampler
metro.out <- metro.sampler(n=1000,import.sampler=gamma.import.sampler,
                           current.param.list=current.param.list,
                           par.start=c(kappa.fit,lambda.fit))

head(metro.out$draws)
metro.out$accept.rate
hist(metro.out$draws[,1],'fd')
hist(metro.out$draws[,2],'fd')


# Try HMC
hmc.out <- hmc.gamma(ndraws=1000,step.size=0.2,nsteps=5,
                     current.param.list=current.param.list,
                     debug=FALSE,last.draw=FALSE)

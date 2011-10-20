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
source(paste(funct.dir,"case_control_samp.R",sep=""))
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


# Fit using true parameters as starting values or crude initializations?
use.true.params <- TRUE


# Load in observed data
filename.doc.topic <- paste(obs.data.dir,"doc_topic_list.RData",sep="")
filename.doc.word.count <- paste(obs.data.dir,"doc_word_count_list.RData",sep="")
filename.feature.word.count <- paste(obs.data.dir,"feature_word_count_list.RData",sep="")
filename.doc.length <- paste(obs.data.dir,"doc_length_table.txt",sep="")

data.list <- process.observed.data(filename.doc.topic=filename.doc.topic,
                                   filename.doc.word.count=filename.doc.word.count,
                                   filename.feature.word.count=filename.feature.word.count,
                                   filename.doc.length=filename.doc.length,L=1)

feature.count.list <- data.list$feature.count.list
doc.count.list <- data.list$doc.count.list
doc.length.vec <- data.list$doc.length.vec
doc.topic.list <- data.list$doc.topic.list

# Initalize parameters
# Load in true parameter values
load("/n/airoldifs1/jbischof/reuters_output/mmm_raw_data/fake_data/mmm_true_params.RData")
if(use.true.params){current.param.list <- true.param.list
                    filename.doc.xi <- paste(obs.data.dir,"doc_xi_list.RData",sep="")
                    load(filename.doc.xi)
                    current.param.list$xi.param.list <- doc.xi.list
                    current.param.list$theta.param.vecs <- current.param.list$theta.param.vecs[names(doc.length.vec),]
                  }

# Initialize parameters
if(!use.true.params){
  #filename.doc.xi <- paste(obs.data.dir,"doc_xi_list.RData",sep="")
  filename.eta.vec <- paste(obs.data.dir,"eta_vec.txt",sep="")
  filename.theta.param.vecs <- paste(obs.data.dir,"initialized_theta.txt",sep="")
  current.param.list <-
    initialize.params(feature.count.list=feature.count.list,
                      doc.count.list=doc.count.list,
                      doc.length.vec=doc.length.vec,
                      doc.topic.list=doc.topic.list,
                      filename.eta.vec=filename.eta.vec,
                      filename.theta.param.vecs=filename.theta.param.vecs,
                      #filename.doc.xi=filename.doc.xi,
                      corpus.topic="CORPUS",
                      topic.address.book=topic.address.book)
  
  ## # For now, borrow hyperparameters from known values
  ## # Alpha being set to uniform vector to ensure unimodal conditional
  ## # posterior for theta.d
  ## alpha.true <- true.param.list$alpha
  ## alpha.use <- rep(1,length(alpha.true))
  ## current.param.list[["alpha"]] <- alpha.use
  ## current.param.list[["nu"]] <- true.param.list$nu
  ## current.param.list[["sigma2"]] <- true.param.list$sigma2
}

# Make sure thetas are sparse before running everything
current.param.list$theta.param.vecs <- as(current.param.list$theta.param.vecs,"sparseMatrix")


job.id <- 1

# Try optimizer with fancy new tau2.max option
system.time(
optim.out <- optim.tree(job.id=job.id,current.param.list=current.param.list,
                        doc.length.vec=doc.length.vec,
                        doc.topic.list=doc.topic.list,
                        feature.count.list=feature.count.list,
                        topic.address.book=topic.address.book,
                        corpus.topic="CORPUS",max.tau2=FALSE,
                        hessian=TRUE,tree.data.out=TRUE,get.prior.hes=TRUE,
                        Ndoc.case.control=NULL)
            )
hes.optim <- optim.out$hessian
par.start <- c(optim.out$mu.f,optim.out$mu.0.f)
tree.data.list <- optim.out$tree.data.list
K <- tree.data.list$K


system.time(
   hes <- eval.mu.hessian(par.start,tree.data.list,diag.only=FALSE,
                          n.sample=NULL)
)

# Compare eigenvalues of matrices
eigen(-hes)$values
eigen(-hes.optim)$values


## current.param.list$theta.param.vecs[job.id,] <- rep(0,K)

system.time(
nr.out <- nr.tree(job.id=job.id,current.param.list=current.param.list,
                  doc.length.vec=doc.length.vec,
                  doc.topic.list=doc.topic.list,
                  feature.count.list=feature.count.list,
                  topic.address.book=topic.address.book,
                  corpus.topic="CORPUS",hessian=FALSE,
                  tree.data.out=FALSE,opt.step=FALSE,
                  hes.diag.only=FALSE,hes.n.sample=NULL)
            )
par.nr <- c(nr.out$mu.f,nr.out$mu.0.f)
hist(par.nr-par.start)
nr.out$value
optim.out$tree.post

tree.import.out <- tree.import.sampler(job.id=job.id,current.param.list=current.param.list,
                                       doc.length.vec=doc.length.vec,
                                       doc.topic.list=doc.topic.list,
                                       feature.count.list=feature.count.list,
                                       topic.address.book=topic.address.book,
                                       ndraws=1000,prop.scale=1)

head(exp(tree.import.out$log.weights))
hist(exp(tree.import.out$log.weights),'fd')
abline(v=1,col="red")

# Run independence chain metropolis sampler
metro.out <- metro.sampler(n=1000,import.sampler=tree.import.sampler,
                            current.param.list=current.param.list,
                            doc.length.vec=doc.length.vec,
                            doc.topic.list=doc.topic.list,
                            feature.count.list=feature.count.list,
                            topic.address.book=topic.address.book,
                            job.id=job.id,
                            par.start=par.start)

head(metro.out$draws)
metro.out$accept.rate
hist(metro.out$draws[,1],'fd')
hist(metro.out$draws[,85],'fd')

# Draw tau2.f vector from the new mu draw
K <- current.param.list$K
mu.new <- metro.out$draws[nrow(metro.out$draws),]
mu.f <- mu.new[1:K]
mu.0.f <- mu.new[K+1]
tau2f.new <- tau2.draw(job.id=job.id,mu.f=mu.f,mu.0.f=mu.0.f,
                       current.param.list=current.param.list)


# Try HMC
system.time(
hmc.draws <- hmc.tree(job.id=job.id,ndraws=10,step.size=0.2,nsteps=5,
                      current.param.list=current.param.list,
                      doc.length.vec=doc.length.vec,
                      doc.topic.list=doc.topic.list,
                      feature.count.list=feature.count.list,
                      topic.address.book=topic.address.book,
                      n.sample.hes=1000,Ndoc.case.control=NULL,
                      debug=FALSE)
            )
index <- 8
plot(hmc.draws[,index],type="b")

# Put HMC draws in current.param.list and run HMC again
current.param.list$mu.param.vecs[job.id,] <- hmc.draws[nrow(hmc.draws),1:K]
current.param.list$mu.corpus.vec[job.id] <- hmc.draws[nrow(hmc.draws),K+1]
mu.f <- current.param.list$mu.param.vecs[job.id,]
mu.0.f <- current.param.list$mu.corpus.vec[job.id]
current.param.list$tau2.param.vecs[job.id,] <- tau2.draw(job.id=job.id,
                                                         mu.f=mu.f,mu.0.f=mu.0.f,
                                                         current.param.list=current.param.list)



# Try to plot log posterior one dimension at a time
mu.plot <- tree.post.proj(index=index,par=par.start,range.points=2,
                          tree.data.list=tree.data.list,
                          npoints=1000,
                          current.param.list=current.param.list,
                          topic.address.book=topic.address.book)
mu.log.dens <- mu.plot[,2]
mu.dens <- exp(mu.log.dens-max(mu.log.dens))
plot(cbind(mu.plot[,1],mu.dens),type="l")
abline(v=par.start[index],col="blue")
abline(v=max(hmc.draws[,index]),col="red")
abline(v=min(hmc.draws[,index]),col="red")
max.point <- which.max(mu.plot[,2])
points(mu.plot[max.point,1],mu.plot[max.point,2],col="red")


library("numDeriv")
# Comparing gradients for log posterior
num.grad.post <- grad(tree.log.posterior,par.start,tree.data.list=tree.data.list,
                      topic.address.book=topic.address.book,
                      parent.child.list=
                      current.param.list$parent.child.list,
                      corpus.topic=corpus.topic,max.tau2=FALSE)
analy.grad.post <- tree.log.post.gradient(par.start,tree.data.list=tree.data.list,
                                          topic.address.book=topic.address.book,
                                          parent.child.list=
                                          current.param.list$parent.child.list,
                                          corpus.topic=corpus.topic,max.tau2=FALSE)
# Comparing gradients for log likelihood
num.grad.like <- grad(tree.log.like,par.start[1:K],
                      tree.data.list=tree.data.list)
analy.grad.like <- eval.like.grad(par.start[1:K],
                      tree.data.list=tree.data.list)


## optim.out <- optim.tree(job.id=job.id,current.param.list=current.param.list,
##                         doc.length.vec=doc.length.vec,
##                         doc.topic.list=doc.topic.list,feature.count.list=feature.count.list,
##                         topic.address.book=topic.address.book,
##                         corpus.topic="CORPUS",hessian=TRUE)

## optim.out
## true.param.list$mu.param.vecs[job.id,]
## true.param.list$tau2.param.vecs[job.id,]
## K <- current.param.list$K
## hessian.mus <- optim.out$hessian[1:(K+1),1:(K+1)]
## cov.tree <- solve(-hessian.mus)


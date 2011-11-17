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
#out.dir <- paste(main.dir,"mmm_fits/fake_data/",sep="")
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
filename.doc.xi <- paste(obs.data.dir,"doc_xi_list.RData",sep="")

load("/n/airoldifs1/jbischof/reuters_output/mmm_raw_data/fake_data/mmm_true_params.RData")
if(use.true.params){
  current.param.list <- true.param.list
  load(filename.doc.xi)
  current.param.list$xi.param.list <- doc.xi.list
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
  ## current.param.list[["lambda2"]] <- true.param.list$lambda2
}

# Many active topics
job.id <- "6"
# Two active topics
job.id <- "7773"
# One active topic
job.id <- "6412"

# Testing out classifier?
classify <- TRUE

# Check optimizer
system.time(
optim.xi.out <- optim.xi(job.id=job.id,
                         current.param.list=current.param.list,
                         doc.length.vec=doc.length.vec,
                         doc.count.list=doc.count.list,
                         doc.topic.list=doc.topic.list,
                         xi.data.out=TRUE,hessian=TRUE,
                         active.only=FALSE,Nfeat.case.control=NULL,
                         classify=classify)
            )

xi.opt <- optim.xi.out$xi.d
xi.data.list <- optim.xi.out$xi.data.list
optim.hes <- optim.xi.out$hessian
hes.like <- optim.hes + xi.data.list$Sigma.inv
eigen(-optim.hes)$values
eigen(-hes.like)$values

xi.opt
true.xi <- true.param.list$xi.param.vecs[job.id,]
hist(xi.opt-true.xi,'fd')
current.param.list$xi.param.list[[job.id]]


# Check importance sampler
xi.import.out <- xi.import.sampler(job.id=job.id,
                                   current.param.list=current.param.list,
                                   doc.length.vec=doc.length.vec,
                                   doc.topic.list=doc.topic.list,
                                   doc.count.list=doc.count.list,
                                   ndraws=10000,prop.scale=1)

head(xi.import.out$weights)
hist(xi.import.out$weights,'fd')
abline(v=1,col="red")


# Run independence chain metropolis sampler
metroh.out <- metro.sampler(n=1000,import.sampler=xi.import.sampler,
                            job.id=job.id,
                            current.param.list=current.param.list,
                            doc.length.vec=doc.length.vec,
                            doc.topic.list=doc.topic.list,
                            doc.count.list=doc.count.list,
                            par.start=xi.opt)

head(metroh.out$draws)
metroh.out$accept.rate
hist(metroh.out$draws[,1],'fd')
hist(metroh.out$draws[,2],'fd')


# Try HMC
library("MCMCpack")
system.time(
hmc.draws <- hmc.xi(job.id=job.id,ndraws=100,step.size=0.05,nsteps=30,
                    current.param.list=current.param.list,
                    doc.length.vec=doc.length.vec,
                    doc.topic.list=doc.topic.list,
                    doc.count.list=doc.count.list,
                    hessian.like=hes.like,
                    active.only=FALSE,
                    debug=FALSE,classify=classify)
            )
index <- 7
active.topics <- xi.data.list$active.topics
index <- active.topics[1]
plot(hmc.draws[,index],type="b")
hist(hmc.draws[,index],'fd')
acf(hmc.draws[,index])

if(classify){
  mean.draws <- colMeans(hmc.draws)
  doc.topics <- doc.topic.list[[job.id]]
  pos.doc.topic <- sapply(doc.topics,function(topic){which(names(mean.draws)==topic)})
  boxplot(mean.draws[pos.doc.topic],mean.draws[-c(pos.doc.topic)])
}

# Put HMC draws in current.param.list and run HMC again
current.param.list$xi.param.vecs[job.id,] <- hmc.draws[nrow(hmc.draws),]


# Try to plot log posterior one dimension at a time
npoints <- 200
xi.proj.out <- xi.post.proj(index=index,par=xi.opt,npoints=npoints,
                            range.points=30,xi.data.list=xi.data.list,
                            type="post")
xi.log.dens <- xi.proj.out$dens
ruler <- xi.proj.out$x
xi.dens <- exp(xi.log.dens-max(xi.log.dens))
plot(cbind(ruler,xi.dens),type="l")
abline(v=xi.opt[index],col="blue")
abline(v=max(hmc.draws[,index]),col="red")
abline(v=min(hmc.draws[,index]),col="red")
hist(hmc.draws[,index],'fd',freq=FALSE,add=TRUE)
#max.point <- which.max(mu.plot[,2])
#points(mu.plot[max.point,1],mu.plot[max.point,2],col="red")


library("numDeriv")
# Comparing gradients for log posterior
num.grad.post <- grad(xi.log.posterior,xi.opt,xi.data.list=xi.data.list,
                      eta.vec=xi.data.list$eta.vec,
                      Sigma.inv=xi.data.list$Sigma.inv,
                      active.only=FALSE)
analy.grad.post <- xi.log.post.gradient(xi.opt,xi.data.list=xi.data.list,
                                        eta.vec=xi.data.list$eta.vec,
                                        Sigma.inv=xi.data.list$Sigma.inv,
                                        active.only=FALSE)

# Comparing gradients for log likelihood
num.grad.like <- grad(xi.w.log.like,xi.opt,
                      xi.data.list=xi.data.list,active.only=FALSE)
analy.grad.like <- xi.w.log.like.grad(xi.opt,xi.data.list=xi.data.list,
                                  active.only=FALSE)



###########################################################
# Plot log posterior in two dimensions

npoints <- 50
active.topics <- xi.data.list$active.topics
topics.plot <- active.topics[1:2]
library("lattice")
label.list=list(wlike="Word log-likelihood",ilike="Label log-likelihood",
  prior="Xi prior",post="Xi posterior")

for(plot.type in c("wlike","ilike","prior","post")){
  xi.proj.out <- xi.post.proj(index=topics.plot,par=xi.opt,npoints=npoints,
                              range.points=30,xi.data.list=xi.data.list,
                              type=plot.type)
  z.vec <- xi.proj.out$dens
  x <- xi.proj.out$x
  y <- xi.proj.out$y
  #z.mat <- matrix(z.vec,npoints,npoints)
  #contour(x=x,y=y,z=z.mat)
  #points(xi.opt[topics.plot[1]],xi.opt[topics.plot[2]])
  
  data.mat.post <- data.frame(x=x,y=y,z=z.vec)
  wire.out <- wireframe(z~x*y,data=data.mat.post,
                        drape=TRUE,scale=list(arrows=FALSE),
                        #screen = list(z = 120, x = -60),
                        main=label.list[[plot.type]],xlab="xi.d.1",
                        ylab="xi.d.2",zlab="value")
  pdf(paste(out.dir,"xi_",plot.type,"_plot.pdf",sep=""),height=6,width=6)
  print(wire.out)
  dev.off()
}



########################################################################
######################### Code graveyard ###############################
########################################################################

## # Try to plot posterior of the xis
## # Need a job id with only two active topics for this code to work


## active.topics <- xi.data.list$active.topics
## topics.plot <- active.topics[1:2]
## eta.vec <- current.param.list$eta.vec[topics.plot]
## lambda2 <- current.param.list$lambda2
## Sigma <- lambda2*diag(2)
## Sigma.inv <- (1/lambda2)*diag(2)

## # Plot of log posterior
## npoints <- 50
## x <- seq(-20,20,length.out=npoints)
## y <- seq(-20,20,length.out=npoints)
## grid <- expand.grid(x,y)
## colnames(grid) <- topics.plot
## z.vec.post <- apply(grid,1,xi.log.posterior,eta.vec=eta.vec,Sigma.inv=Sigma.inv,
##            xi.data.list=xi.data.list,active.only=TRUE)
## contour(x=x,y=y,z=z.mat.post)
## points(xi.opt[1],xi.opt[2])

## library("lattice")
## data.mat.post <- data.frame(x=grid[,1],y=grid[,2],z=z.vec.post)
## wireframe(z~x*y,data=data.mat.post,drape=TRUE,scale=list(arrows=FALSE))


## # Plot of w log likelihood
## npoints <- 50
## x <- seq(-20,10,length.out=npoints)
## y <- seq(-20,10,length.out=npoints)
## grid <- expand.grid(x,y)
## colnames(grid) <- topics.plot
## z.vec.wlike <- apply(grid,1,xi.w.log.like,xi.data.list=xi.data.list,active.only=TRUE)
## z.mat.wlike <- matrix(z.vec.wlike,npoints,npoints)
## contour(x=x,y=y,z=z.mat.wlike)
## points(xi.opt[1],xi.opt[2])

## library("lattice")
## data.mat.wlike <- data.frame(x=grid[,1],y=grid[,2],z=z.vec.wlike)
## wireframe(z~x*y,data=data.mat.wlike,drape=TRUE,scale=list(arrows=FALSE))


## # Plot of I log likelihood
## I.vec <- xi.data.list$I.doc.vec
## npoints <- 50
## x <- seq(-20,10,length.out=npoints)
## y <- seq(-20,10,length.out=npoints)
## grid <- expand.grid(x,y)
## colnames(grid) <- topics.plot
## z.vec.ilike <- apply(grid,1,xi.label.log.like,I.vec=I.vec)
## z.mat.ilike <- matrix(z.vec.ilike,npoints,npoints)
## contour(x=x,y=y,z=z.mat.ilike)
## points(xi.opt[1],xi.opt[2])

## library("lattice")
## data.mat.ilike <- data.frame(x=grid[,1],y=grid[,2],z=z.vec.ilike)
## wireframe(z~x*y,data=data.mat.ilike,drape=TRUE,scale=list(arrows=FALSE))


## # Plot of total likelihood
## z.vec.like <- z.vec.wlike + z.vec.ilike
## z.mat.like <- z.mat.wlike + z.mat.ilike
## contour(x=x,y=y,z=z.mat.like)
## points(xi.opt[1],xi.opt[2])

## data.mat.like <- data.frame(x=grid[,1],y=grid[,2],z=z.vec.like)
## wireframe(z~x*y,data=data.mat.like,drape=TRUE,scale=list(arrows=FALSE))

# Scripts to initialize model parameters from observed data
library("Matrix")

# Get parent.child.list from topic.address.book
get.parent.child.list <- function(topic.address.book){
  parent.child.list <- list()
  parent.list <- topic.address.book[,"parent"]
  topic.list <- topic.address.book[,"topic"]
  for(parent in unique(parent.list)){
    child.pos <- which(parent.list==parent)
    children <- topic.list[child.pos]
    names(children) <- child.pos
    parent.child.list[[parent]] <- children
  }
  return(parent.child.list)
}


# Function to generate inital values for parameters
initialize.params <- function(feature.count.list,doc.count.list,
                              doc.length.vec,doc.topic.list,
                              topic.address.book,
                              filename.mu.param.vecs,
                              filename.mu.corpus.vec,
                              filename.tau2.vec,
                              filename.doc.xi=NULL,
                              filename.eta.vec,
                              filename.theta.param.vecs,
                              corpus.topic="CORPUS",
                              lambda2.start=4,
                              eta.offset=0,
                              scale.Sigma.0=4,
                              kappa.0=10,
                              omega2.0=10,
                              full.Sigma=FALSE){
  
  # Get list of topics
  topics <- topic.address.book[,"topic"]
  tree.parent.topics <- unique(topic.address.book[,"parent"])
  corpus.topic.pos <- which(tree.parent.topics==corpus.topic)

  # Get basic info on corpus
  word.ids <- names(feature.count.list)
  doc.ids <- names(doc.length.vec)
  D <- length(doc.ids)
  K <- length(topics)
  K.parent <- length(tree.parent.topics)
  V <- length(word.ids)
  topic.pos <- 1:K
  names(topic.pos) <- topics

  # Load in initialized doc memb parameters
  # Thetas already saved in sparse representation
  load(filename.theta.param.vecs)
  # Make sure theta.param.vecs is in same order as doc.length.vec
  # so that exposure factor multiplication works
  theta.param.vecs <- theta.param.vecs[doc.ids,]

  
  # Load in initialized tree parameters
  
  # Load in initialized mu param vecs
  mu.param.vecs <- as.matrix(read.table(filename.mu.param.vecs,as.is=TRUE,
                                        row.names = 1,header=TRUE))[word.ids,]
  # Load in initialized corpus mu vector
  mu.corpus.vec <- as.matrix(read.table(filename.mu.corpus.vec,
                                  as.is = TRUE, row.names = 1))[,1][word.ids]
  # Load in initialized tau2 vector
  tau2.vec <- as.matrix(read.table(filename.tau2.vec,
                                   as.is = TRUE, row.names = 1))[,1][word.ids]
  # Create tau2.param.vecs
  tau2.param.vecs <- matrix(tau2.vec,nrow=V,ncol=K.parent,byrow=FALSE,
                            dimnames=list(names(tau2.vec),tree.parent.topics))
  
  
  # Load in initialized eta vector
  eta.vec <- as.matrix(read.table(filename.eta.vec,
                                  as.is = TRUE, row.names = 1))[,1]
  # Make sure eta.vec in same order as thetas
  eta.vec <- eta.vec[colnames(theta.param.vecs)]
  # Use eta offset
  eta.vec <- eta.vec + eta.offset
  
  # Initialize xi.param.vecs with eta.vec; again, rows should be same
  # order as doc.length.vec
  xi.param.vecs <- matrix(eta.vec,nrow=D,ncol=K,byrow=TRUE,
                          dimnames=list(doc.ids,names(eta.vec)))

  # Get the parent.child.list
  parent.child.list <- get.parent.child.list(topic.address.book)

  # Initialize mu hyperparameters
  psi <- mean(mu.corpus.vec)
  gamma2 <- var(mu.corpus.vec)

  # Initialize tau2 hyperparameters (must line up with initialized tau2s
  # for importance sampler to work)
  # Note that only have one unique initialized tau2 for every word
  optim.out <- profile.optim.gamma(tau2.param.vecs[,1])
  inv.chisq.opt <- convert.hparams(par=c(optim.out$kappa,optim.out$lambda))
  nu <- inv.chisq.opt[1]
  sigma2 <- inv.chisq.opt[2]
  
  # Figure out if full Sigma matrix requested and then create one
  # Ideally this will eventually be initialized as a scaled up
  # version of the empirical cov matrix of the topic indicators
  if(full.Sigma){
    Sigma <- lambda2.start*diag(K)
    Sigma.0 <- scale.Sigma.0*diag(K)
  } else {Sigma.0 <- Sigma <- NULL}
  
  # Return list of initialized parameters
  current.param.list <- list(theta.param.vecs=theta.param.vecs,
                             mu.param.vecs=mu.param.vecs,
                             mu.corpus.vec=mu.corpus.vec,
                             tau2.param.vecs=tau2.param.vecs,
                             xi.param.vecs=xi.param.vecs,
                             K=K,D=D,V=V,
                             psi=psi,gamma=sqrt(gamma2),
                             lambda2=lambda2.start,
                             full.Sigma=full.Sigma,Sigma=Sigma,
                             Sigma.0=Sigma.0,kappa.0=kappa.0,
                             omega2.0=omega2.0,
                             eta.vec=eta.vec,
                             nu=nu,sigma2=sigma2,
                             parent.child.list=parent.child.list)

  # Load in xi.param.list if requested
  if(!(is.null(filename.doc.xi))){
    load(filename.doc.xi)
    current.param.list$xi.param.list <- doc.xi.list
  }
  
  return(current.param.list)
}


# Function to set blocks of jobs and output the document/word data
# needed by each slave
get.job.lists.and.data <- function(doc.count.list.orig,
                                   doc.topic.list.orig,
                                   doc.length.vec,
                                   theta.param.vecs,
                                   n.slaves,
                                   mu.corpus.vec=NULL,
                                   feature.count.list.orig=NULL,
                                   slave.file.root="slave_data",
                                   verbose=FALSE,
                                   classify=FALSE,
                                   active.docs.only=FALSE){

  # Get indexes to cycle through
  if(!classify){word.ids <- names(mu.corpus.vec)}
  doc.ids <- rownames(theta.param.vecs)

  if(active.docs.only){
    # Figure out which theta.param.vecs need to be updated (have more than
    # one active topic)
    active.docs <- apply(theta.param.vecs,1,
                         function(vec){length(which(vec>0))>1})
    # Only update active docs
    doc.ids <- doc.ids[active.docs]
  }

  # Get block sizes for parameters
  #n.slaves <- mpi.comm.size(comm=0)-1
  # Get block size for tree
  if(!classify){n.words <- length(word.ids)
                mpi.tree.block.size <- ceiling(n.words/n.slaves)}
  # Get block size for xis
  n.docs <- length(doc.ids)
  mpi.xi.block.size <- ceiling(n.docs/n.slaves)

  
  # Divide list of tasks into blocks of mpi.xi.block.size chunks
  # How many blocks?
  remainder.xi <- n.docs %% mpi.xi.block.size > 0
  njobs.xi <- trunc(n.docs/mpi.xi.block.size) +
    ifelse(remainder.xi,1,0)
  
  # Create job list for xis
  xi.job.list <- list()
  for(pos in 1:njobs.xi){
    pos.do <- c(1:mpi.xi.block.size)+(pos-1)*mpi.xi.block.size
    pos.do <- pos.do[pos.do <= n.docs]
    xi.job.list[[toString(pos)]] <- doc.ids[pos.do]
  }
  
  if(!classify){
    # Divide list of tasks into blocks of mpi.tree.block.size chunks
    # How many blocks?
    remainder.tree <- n.words %% mpi.tree.block.size > 0
    njobs.tree <- trunc(n.words/mpi.tree.block.size) +
      ifelse(remainder.tree,1,0)
    
    # Create job list for tree 
    tree.job.list <- list()
    for(pos in 1:njobs.tree){
      pos.do <- c(1:mpi.tree.block.size)+(pos-1)*mpi.tree.block.size
      pos.do <- pos.do[pos.do <= n.words]
      tree.job.list[[toString(pos)]] <- word.ids[pos.do]
    }
  }
  
  # Print worker assignments if desired
  if(verbose){
    if(!classify){
      cat(paste(njobs.tree,"tree worker ids:\n"))
      print(names(tree.job.list))
    }
    cat(paste(njobs.xi,"xi worker ids:\n"))
    print(names(xi.job.list))
  }
  
  # Now write a data file for each slave
  for(slave.id in 1:n.slaves){
    slave.id.str <- toString(slave.id)
    file.out <- paste(slave.file.root,slave.id,".RData",sep="")
    doc.count.list <- doc.count.list.orig[xi.job.list[[slave.id.str]]]
    doc.topic.list <- doc.topic.list.orig[xi.job.list[[slave.id.str]]]
    
    if(classify){save(doc.count.list,doc.length.vec,doc.topic.list,file=file.out)}
    
    if(!classify){
      feature.count.list <- feature.count.list.orig[tree.job.list[[slave.id.str]]]
      save(doc.count.list,doc.topic.list,doc.length.vec,feature.count.list,
           file=file.out)
    }
  }
  
  out.list <- list(xi.job.list=xi.job.list)
  if(!classify){out.list$tree.job.list <- tree.job.list}
  
  return(out.list)
}

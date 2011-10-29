# Scripts to initialize model parameters from observed data

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
                              scale.Sigma.0=4,
                              kappa=10,
                              full.Sigma=FALSE){
  
  # Get list of topics and create theta vectors
  topics <- topic.address.book[,"topic"]
  tree.parent.topics <- unique(topic.address.book[,"parent"])
  corpus.topic.pos <- which(tree.parent.topics==corpus.topic)

  doc.names <- names(doc.count.list)
  D <- length(doc.names)
  K <- length(topics)
  K.parent <- length(tree.parent.topics)
  V <- length(feature.count.list)
  topic.pos <- 1:K
  names(topic.pos) <- topics

  # Load in initialized doc memb parameters
  # Save theta.param.vecs in sparse representation
  theta.param.vecs <- read.table(filename.theta.param.vecs,as.is=TRUE)
  theta.param.vecs <- as(as.matrix(theta.param.vecs), "sparseMatrix")
  # Make sure theta.param.vecs is in same order as doc.length.vec
  # so that exposure factor multiplication works
  theta.param.vecs <- theta.param.vecs[names(doc.length.vec),]

  
  # Load in initialized tree parameters
  
  # Load in initialized mu param vecs
  mu.param.vecs <- as.matrix(read.table(filename.mu.param.vecs,as.is=TRUE,
                                        row.names = 1,header=TRUE))
  # Load in initialized corpus mu vector
  mu.corpus.vec <- as.matrix(read.table(filename.mu.corpus.vec,
                                  as.is = TRUE, row.names = 1))[,1]
  # Load in initialized tau2 vector
  tau2.vec <- as.matrix(read.table(filename.tau2.vec,
                                   as.is = TRUE, row.names = 1))[,1]
  # Create tau2.param.vecs
  tau2.param.vecs <- matrix(tau2.vec,nrow=V,ncol=K.parent,byrow=FALSE,
                            dimnames=list(names(tau2.vec),tree.parent.topics))
  
  
  # Load in initialized eta vector
  eta.vec <- as.matrix(read.table(filename.eta.vec,
                                  as.is = TRUE, row.names = 1))[,1]
  # Make sure eta.vec in same order as thetas
  eta.vec <- eta.vec[colnames(theta.param.vecs)]
  
  # Initialize xi.param.vecs with eta.vec; again, rows should be same
  # order as doc.length.vec
  xi.param.vecs <- matrix(eta.vec,nrow=D,ncol=K,byrow=TRUE,
                          dimnames=list(names(doc.length.vec),
                            names(eta.vec)))
  

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
                             Sigma.0=Sigma.0,kappa=kappa,
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



# Script to generate the parameters of the MMM

gen.mmm.params <- function(nchildren,nlevels,ndocs,nwords,
                           psi,gamma,nu,sigma2,
                           eta.vec=NULL,lambda2=NULL,
                           gp="dirichlet",verbose=FALSE){

  # Translate function inputs into model's inputs
  J <- nchildren
  D <- ndocs
  V <- nwords
  
  # Total number of active topics (this determined by nlevels and nchildren)
  K <- count.active.topics(nlevels,J)

  # Generate alpha vector
  alpha <- as.vector(rdirichlet(1,rep(1,K)))

  # Generate theta parameters
  Sigma <- lambda2*diag(K)
  theta.out <- gen.theta.param.vecs(alpha=alpha,eta.vec=eta.vec,Sigma=Sigma,
                                    D=D,gp=gp,verbose=verbose)
  theta.param.vecs <- theta.out$theta.param.vecs
  I.vecs <- theta.out$I.vecs
  xi.param.vecs <- theta.out$xi.vecs

  # For now, get rid of documents without assigned labels

  # Draw list of tau2s for every word in vocabulary
  tau2.param.list <- tau2.param.gen(V=V,nchild=J,nu=nu,sigma2=sigma2)
  tau2.param.vecs <- get.tau2.matrix(tau2.param.list)
  rownames(tau2.param.vecs) <- 1:nrow(tau2.param.vecs)
  colnames(tau2.param.vecs)[1] <- "CORPUS"
  
  # Draw list of mus for every word in vocabulary
  mu.param.list <- mu.param.gen(nchild=J,psi=psi,gamma=gamma,
                                tau2.param.list=tau2.param.list)
  
  # Get vector of rates for each feature that will generate the counts
  mu.params.out <- get.mu.matrix(mu.param.list)
  mu.param.vecs <- mu.params.out$mu.param.vecs
  rownames(mu.param.vecs) <- 1:V
  mu.corpus.vec <- mu.params.out$mu.corpus.vec
  names(mu.corpus.vec) <- 1:V
  
  # Get labeled topics for each document
  topics <- colnames(mu.param.vecs)
  names(alpha) <- topics
  colnames(theta.param.vecs) <- colnames(xi.param.vecs) <- topics
  rownames(theta.param.vecs) <-rownames(xi.param.vecs) <- 1:nrow(theta.param.vecs)
  
  # Create data address book and parent.child.list
  topic.address.book <- gen.topic.address.book(topics)
  parent.child.list <- get.parent.child.list(topic.address.book)

  true.param.list <- list(alpha=alpha,mu.param.vecs=mu.param.vecs,
                          mu.corpus.vec=mu.corpus.vec,
                          tau2.param.vecs=tau2.param.vecs,
                          theta.param.vecs=theta.param.vecs,
                          I.vecs=I.vecs,xi.param.vecs=xi.param.vecs,
                          K=K,D=D,V=V,psi=psi,gamma=gamma,nu=nu,
                          sigma2=sigma2,parent.child.list=parent.child.list)

  if(any(gp=="logit.norm",gp=="mv.probit")){
    names(eta.vec) <- topics
    true.param.list$eta.vec <- eta.vec
    true.param.list$lambda2 <- lambda2
  }

  return(list(true.param.list=true.param.list,
              topic.address.book=topic.address.book))
}

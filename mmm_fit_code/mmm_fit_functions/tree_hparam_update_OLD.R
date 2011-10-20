# Functions for analytic updates of tree hyperparameters mu.f.0 and tau2.f

# Function to apply analytic update to corpus level mu
mu.corpus.update <- function(psi,gamma,mu.f,tau2f.vec,
                             parent.child.list,
                             corpus.topic){
  topic <- corpus.topic
  child.topics <- parent.child.list[[topic]]
  pos.children <- as.numeric(names(child.topics))
  mu.children <- mu.f[pos.children]
  tau2.self <- tau2f.vec[corpus.topic]
  nchild <- length(mu.children)
  child.sum <- sum(mu.children)
  numer <- (child.sum/tau2.self)+(psi/gamma^2)
  denom <- (nchild/tau2.self)+(1/gamma^2)
  mu.0.f <- numer/denom
  return(mu.0.f)
}

# Function to apply analytic update to tau2 parameters
tau2.update <- function(topic,nu,sigma2,mu.0.f,mu.f,
                        parent.child.list,corpus.topic){
  
  topic <- toString(topic)
  
  # Get own mu
  if(topic==corpus.topic){mu.self <- mu.0.f}
  else{
    # Figure out parent topic
    pos.topic.address.book <- which(topic.address.book[,"topic"]==topic)
    parent <- topic.address.book[pos.topic.address.book,"parent"]
    mu.self <- mu.f[topic]
  }
  
  # Get child mus
  child.topics <- parent.child.list[[topic]]
  pos.children <- as.numeric(names(child.topics))
  mu.children <- mu.f[pos.children]
  nchild <- length(mu.children)
  
  # Calculate new tau2.fk
  deviat <- sum((mu.children-mu.self)^2)
  numer <- deviat + nu*sigma2
  denom <- nchild + nu + 2
  tau2.new <- numer/denom
  return(tau2.new)
}

update.tree.hparams <- function(mu.f,current.param.list,mu.data.list,
                                parent.child.list,topic.address.book,
                                corpus.topic="CORPUS"){

  # Unpack parameters from respective lists
  psi <- current.param.list$psi
  gamma <- current.param.list$gamma
  tau2f.vec <- mu.data.list$tau2f.vec
  nu <- current.param.list$nu
  sigma2 <- current.param.list$sigma2
  
  mu.0.f <- mu.corpus.update(psi=psi,gamma=gamma,
                             mu.f=mu.f,tau2f.vec=tau2f.vec,
                             parent.child.list=parent.child.list,
                             corpus.topic=corpus.topic)

  # Get full list of topics for the tau2 parameters
  # This way updated parameters will stay in same order
  topics <- names(tau2f.vec)

  # Update tau2 params for each topic
  tau2f.vec <- sapply(topics,tau2.update,nu=nu,
                      sigma2=sigma2,mu.0.f=mu.0.f,mu.f=mu.f,
                      parent.child.list=parent.child.list,
                      corpus.topic=corpus.topic)

  hparam.list <- list(mu.0.f=mu.0.f,tau2f.vec=tau2f.vec)
  return(hparam.list)
}

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


# Function to get initial values for parameters using observed data
get.theta.d <- function(doc.topic.vec,theta.base,topic.pos){
  # Get active topics from document
  active.pos <- topic.pos[doc.topic.vec]
  # How many active topics? 
  n.active <- length(doc.topic.vec)
  # Initialize theta to be uniform vector over active topics
  theta.active <- 1/n.active
  theta.base[active.pos] <- theta.active
  return(theta.base)
}

# Function to get total exposure for each topic assuming
# equal doc memberships
get.topic.exposures <- function(doc.length.vec,doc.topic.list,
                                n.doc.topics,topic.pos,doc.names){
  K <- length(topic.pos)
  topic.expose <- rep(0,K)
  names(topic.expose) <- names(topic.pos)
  doc.expose.contrib <- c()

  for(i in 1:length(doc.length.vec)){
    doc.id <- names(doc.length.vec)[i]
    length.doc <- doc.length.vec[i]
    topics.doc <- doc.topic.list[[doc.id]]
    pos.doc.topics <- topic.pos[topics.doc]
    expose.contrib <- 1/n.doc.topics[i]
    #print(paste("doc.id",doc.id))
    #print(paste("length.doc",length.doc))
    #print(paste("topics.doc",topics.doc))
    #print(paste("pos.doc.topics",pos.doc.topics))
    
    doc.expose.contrib <- c(doc.expose.contrib,expose.contrib)
    topic.expose[pos.doc.topics] <- topic.expose[pos.doc.topics] +
      expose.contrib*length.doc
  }

  # Smooth out any topics with no exposure
  topic.expose[topic.expose==0] <- mean(topic.expose)

  names(doc.expose.contrib) <- doc.names
  return(list(topic.expose=topic.expose,doc.expose.contrib=doc.expose.contrib))
}

# Function to get total count for each topic assuming
# equal doc memberships
get.topic.word.counts <- function(feature.count.list,doc.expose.contrib,
                                  doc.names,topic.pos){
  # Take feature.count.list by feature, divide each doc-count by expose param,
  # and sum vector. If sum=0, set to some low number to smooth estimates.
  K <- length(topic.pos)
  feature.topic.count.list <- list()
   
  for(i in 1:length(feature.count.list)){
    # Create empty list of topic counts for feature
    feature.topic.count <- rep(0,K)
    names(feature.topic.count) <- names(topic.pos)
    word.id <- names(feature.count.list)[i]
    # Get the doc-specific counts for the feature
    counts.feature <- feature.count.list[[i]]
    # Get the ids of the docs with positive feature counts
    doc.ids <- names(counts.feature)
    # Get the # of counts to divide between topics in document
    norm.counts.feature <- counts.feature*doc.expose.contrib[doc.ids]
    # Get relevant portion of doc.topic.list (docs with positive counts)
    topics.docs <- doc.topic.list[doc.ids]
    # Loop through all documents with positive counts and add divided count
    # to each of document's topics
    for(j in 1:length(doc.ids)){
      # Get the id for document j
      doc.id <- names(topics.docs)[j]
      # Topics for document j
      topics.doc <- doc.topic.list[[doc.id]]
      # Get position of these topics in the vector of topic counts
      pos.doc.topics <- topic.pos[topics.doc]
      # Add divided counts to relevant spots in vector
      feature.topic.count[topics.doc] <- feature.topic.count[topics.doc] +
        norm.counts.feature[j]
    }
    # If any counts are zero, round them up to one
    feature.topic.count[feature.topic.count<1] <- 1
    # Now add total feature-topic counts to list
    feature.topic.count.list[[word.id]] <- feature.topic.count
  }
  return(feature.topic.count.list)
}


# Function to generate inital values for parameters
initialize.params <- function(feature.count.list,doc.count.list,
                              doc.length.vec,doc.topic.list,
                              topic.address.book,
                              #filename.doc.xi,
                              filename.eta.vec,
                              filename.theta.param.vecs,
                              corpus.topic="CORPUS",
                              lambda2.start=4){
  
  # Get list of topics and create theta vectors
  topics <- topic.address.book[,"topic"]
  tree.parent.topics <- unique(topic.address.book[,"parent"])
  corpus.topic.pos <- which(tree.parent.topics==corpus.topic)
  tree.parent.topics <- tree.parent.topics[-corpus.topic.pos]
  # Save theta.param.vecs in sparse representation
  theta.param.vecs <- read.table(filename.theta.param.vecs,as.is=TRUE)
  theta.param.vecs <- as(as.matrix(theta.param.vecs), "sparseMatrix")
  # Make sure theta.param.vecs is in same order as doc.length.vec
  # so that exposure factor multiplication works
  theta.param.vecs <- theta.param.vecs[names(doc.length.vec),]

  doc.names <- names(doc.count.list)
  D <- length(doc.names)
  K <- length(topics)
  V <- length(feature.count.list)
  topic.pos <- 1:K
  names(topic.pos) <- topics
  ## theta.base <- rep(0,K)
  ## theta.param.vecs <- t(sapply(doc.topic.list,get.theta.d,theta.base=theta.base,
  ##                      topic.pos=topic.pos))
  ## colnames(theta.param.vecs) <- topics
  ## rownames(theta.param.vecs) <- doc.names
  ## load(filename.doc.xi)

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

  # Get number of topics for each doc
  n.doc.topics <- sapply(doc.topic.list,length)
  names(n.doc.topics) <- doc.names

  # Get total exposure for each topic
  topic.expose.out <- get.topic.exposures(doc.length.vec=doc.length.vec,
                                          doc.topic.list=doc.topic.list,
                                          n.doc.topics=n.doc.topics,
                                          topic.pos=topic.pos,doc.names=doc.names)
  topic.expose <- topic.expose.out$topic.expose
  doc.expose.contrib <- topic.expose.out$doc.expose.contrib

  # Get total word count for each topic
  feature.topic.count.list <- get.topic.word.counts(feature.count.list,
                                                    doc.expose.contrib,
                                                    doc.names,topic.pos)

  # Get the parent.child.list
  parent.child.list <- get.parent.child.list(topic.address.book)
  
  # Get estimated mus for each feature
  mu.param.vecs <- t(sapply(feature.topic.count.list,
                          function(feature.topic.count.vec,topic.expose){
                            mu.f.vec <- log(feature.topic.count.vec)-
                              log(topic.expose)
                            return(mu.f.vec)},
                          topic.expose=topic.expose))

  mu.corpus.vec <- apply(mu.param.vecs,1,mean)
  names(mu.corpus.vec) <- rownames(mu.param.vecs)
  tau2.param.vecs <- t(apply(mu.param.vecs,1,
                             function(mu.f.vec){
                               rep(var(mu.f.vec),length(tree.parent.topics)+1)}))
  colnames(mu.param.vecs) <- topics
  colnames(tau2.param.vecs) <- c(corpus.topic,tree.parent.topics)

  # Initialize mu hyperparameters
  psi <- mean(mu.corpus.vec)
  gamma2 <- var(mu.corpus.vec)

  # Initialize tau2 parameters (must line up with initialized tau2s
  # for importance sampler to work)
  # Note that only have one unique initialized tau2 for every word
  optim.out <- profile.optim.gamma(tau2.param.vecs[,1])
  inv.chisq.opt <- convert.hparams(par=c(optim.out$kappa,optim.out$lambda))
  nu <- inv.chisq.opt[1]
  sigma2 <- inv.chisq.opt[2]
  
  # Return list of initialized parameters
  current.param.list <- list(theta.param.vecs=theta.param.vecs,
                             #xi.param.list=doc.xi.list,
                             mu.param.vecs=mu.param.vecs,
                             mu.corpus.vec=mu.corpus.vec,
                             tau2.param.vecs=tau2.param.vecs,
                             xi.param.vecs=xi.param.vecs,
                             K=K,D=D,V=V,
                             psi=psi,gamma=sqrt(gamma2),
                             lambda2=lambda2.start,
                             eta.vec=eta.vec,
                             nu=nu,sigma2=sigma2,
                             parent.child.list=parent.child.list)
  
  return(current.param.list)
}



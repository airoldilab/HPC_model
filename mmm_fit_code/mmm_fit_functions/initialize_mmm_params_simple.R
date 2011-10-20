# Script to load up the initialized values of mmm parameters using
# the simple model fit and get data

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
initialize.mmm.params <- function(filename.mu.corpus,
                                  filename.mu.param.vecs,
                                  filename.tau2.param.vecs,
                                  filename.theta.param.vecs,
                                  filename.doc.xi.list,
                                  nu=40,sigma2=0.01,
                                  feature.count.list,doc.count.list,
                                  doc.length.vec,doc.topic.list,
                                  corpus.topic="CORPUS",topic.address.book,
                                  theta.sparse=FALSE){

  # Load in initialized parameters
  mu.corpus.table <- read.table(filename.mu.corpus,as.is=TRUE)
  mu.corpus.vec <- mu.corpus.table[,2]
  names(mu.corpus.vec) <- mu.corpus.table[,1]
  mu.param.vecs <- read.table(filename.mu.param.vecs,as.is=TRUE)
  tau2.param.vecs <- read.table(filename.tau2.param.vecs,as.is=TRUE)
  load(filename.doc.xi.list)

  if(theta.sparse){load(filename.theta.param.vecs)
  } else{
    # Save theta.param.vecs in sparse representation
    theta.param.vecs <- read.table(filename.theta.param.vecs,as.is=TRUE)
    theta.param.vecs <- as(as.matrix(theta.param.vecs), "sparseMatrix")
  }

  # Make sure theta.param.vecs is in same order as doc.length.vec
  # so that exposure factor multiplication works
  theta.param.vecs <- theta.param.vecs[names(doc.length.vec),]

  # Make sure the mu and theta param matrices have same column order
  topics <- topic.address.book[,"topic"]
  theta.param.vecs <- theta.param.vecs[,topics]
  mu.param.vecs <- mu.param.vecs[,topics]
  
  # Get data sizes
  K <- length(topics)
  V <- nrow(mu.param.vecs)
  D <- nrow(theta.param.vecs)

  # Get the parent.child.list
  parent.child.list <- get.parent.child.list(topic.address.book)

  # Initialize tree hyperparameters
  psi <- mean(mu.corpus.vec)
  gamma2 <- var(mu.corpus.vec)
  alpha <- rep(1,K)
  
  # Return list of initialized parameters
  current.param.list <- list(theta.param.vecs=theta.param.vecs,
                             xi.param.list=doc.xi.list,
                             mu.param.vecs=as.matrix(mu.param.vecs),
                             mu.corpus.vec=mu.corpus.vec,
                             tau2.param.vecs=as.matrix(tau2.param.vecs),
                             K=K,D=D,V=V,
                             alpha=alpha,nu=nu,sigma2=sigma2,
                             psi=psi,gamma=sqrt(gamma2),
                             parent.child.list=parent.child.list)
  
  return(current.param.list)
}


# Function to set blocks of jobs and output the document/word data
# needed by each slave
get.job.lists.and.data <- function(doc.count.list.orig,
                                   doc.topic.list.orig,
                                   doc.length.vec,
                                   theta.param.vecs,
                                   n.slaves,
                                   feature.count.list.orig=NULL,
                                   slave.file.root="slave_data",
                                   verbose=FALSE,
                                   classify=FALSE){

  # Get indexes to cycle through
  if(!classify){word.ids <- names(feature.count.list.orig)}
  doc.ids <- names(doc.count.list.orig)

  # Figure out which theta.param.vecs need to be updated (have more than
  # one active topic)
  active.docs <- apply(theta.param.vecs,1,
                       function(vec){length(which(vec>0))>1})
  # Only update active docs
  doc.ids <- doc.ids[active.docs]

  # Get block sizes for parameters
  #n.slaves <- mpi.comm.size(comm=0)-1
  # Get block size for tree
  if(!classify){n.words <- length(word.ids)
                mpi.tree.block.size <- ceiling(n.words/n.slaves)}
  # Get block size for thetas
  n.docs <- length(doc.ids)
  mpi.theta.block.size <- ceiling(n.docs/n.slaves)

  
  # Divide list of tasks into blocks of mpi.theta.block.size chunks
  # How many blocks?
  remainder.theta <- n.docs %% mpi.theta.block.size > 0
  njobs.theta <- trunc(n.docs/mpi.theta.block.size) +
    ifelse(remainder.theta,1,0)

  # Create job list for thetas
  theta.job.list <- list()
  for(pos in 1:njobs.theta){
    pos.do <- c(1:mpi.theta.block.size)+(pos-1)*mpi.theta.block.size
    pos.do <- pos.do[pos.do <= n.docs]
    theta.job.list[[toString(pos)]] <- doc.ids[pos.do]
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
    cat(paste(njobs.theta,"theta worker ids:\n"))
    print(names(theta.job.list))
  }
  
  # Now write a data file for each slave
  for(slave.id in 1:n.slaves){
    slave.id.str <- toString(slave.id)
    file.out <- paste(slave.file.root,slave.id,".RData",sep="")
    doc.count.list <- doc.count.list.orig[theta.job.list[[slave.id.str]]]
    doc.topic.list <- doc.topic.list.orig[theta.job.list[[slave.id.str]]]

    if(classify){save(doc.count.list,doc.topic.list,doc.length.vec,
                      file=file.out)}
    
    if(!classify){feature.count.list <- feature.count.list.orig[tree.job.list[[slave.id.str]]]
      save(doc.count.list,doc.topic.list,doc.length.vec,feature.count.list,
           file=file.out)}
      
    ## save(feature.count.list,doc.count.list,doc.topic.list,
    ##      doc.length.vec,file=file.out)
  }
  
  out.list <- list(theta.job.list=theta.job.list)
  if(!classify){out.list$tree.job.list <- tree.job.list}
  
  return(out.list)
}

# Script to classify previously unseen documents based on fit of
# mmm model to training set

# Function to remove words in valid/test data not seen in training data
remove.new.words <- function(doc.count.list,words.train){
  doc.count.list <- lapply(doc.count.list,function(count.vec,words.train){
    active.words <- names(count.vec)
    check.train <- active.words %in% words.train
    if(!all(check.train)){count.vec <- count.vec[check.train]}
    return(count.vec)
  }
                           ,words.train=words.train)

  return(doc.count.list)
}

gen.class.data <- function(ave.param.list,doc.ids,
                           topic.address.book){

  # Get expected eta vec for theta initialization 
  eta.vec <- ave.param.list$eta.vec
  theta.init <- get.theta.from.xi(eta.vec)

  # Set up theta.param.vecs for unseen documents
  # Initialize at uniform vector
  # Get number of topics and documents
  K <- ave.param.list$K
  topics <- topic.address.book[,"topic"]
  ndocs <- length(doc.ids)
  theta.param.vecs <- matrix(theta.init,nrow=ndocs,ncol=K,byrow=TRUE,
                             dimnames=list(doc.ids,topics))
  xi.param.vecs <- matrix(eta.vec,nrow=ndocs,ncol=K,byrow=TRUE,
                          dimnames=list(doc.ids,topics))
  ave.param.list$theta.param.vecs <- theta.param.vecs
  ave.param.list$xi.param.vecs <- xi.param.vecs

  return(ave.param.list)
}

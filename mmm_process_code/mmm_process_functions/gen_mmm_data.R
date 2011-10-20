# Script to generate data from the MMM given the true parameters

gen.mmm.data <- function(true.param.list,ndocs,lda.filename,
                         doc.ids=NULL,shape=10,verbose=TRUE){

  # Extract needed parameters
  D <- ndocs
  alpha <- true.param.list$alpha
  theta.param.vecs <- true.param.list$theta.param.vecs
  doc.norm.lengths <- true.param.list$doc.norm.lengths
  mu.param.vecs <- true.param.list$mu.param.vecs
  topics <- colnames(mu.param.vecs)
  I.vecs <- true.param.list$I.vecs
  V <- true.param.list$V

  # Get indices for docs
  if(is.null(doc.ids)){doc.ids <- 1:D}
  else{if(length(doc.ids)!=D){stop("doc.ids must be same length as number of requested documents")}}

  # Generate lengths for new documents
  doc.norm.lengths <- doc.lgen(D=D,shape=shape)
  names(doc.norm.lengths) <- doc.ids

  # Calculate mean rates for each word in each document
  rate.word.doc.mat <- doc.norm.lengths*theta.param.vecs%*%t(exp(mu.param.vecs))
  rownames(rate.word.doc.mat) <- doc.ids
  colnames(rate.word.doc.mat) <- 1:V
  #print(mean(mu.param.vecs))
  #print(mean(rate.word.doc.mat))

  # Draw matrix of observed word counts
  count.word.doc.mat <- t(apply(rate.word.doc.mat,1,rpois,n=V))
  tot.counts <- colSums(count.word.doc.mat)
  if(verbose){
    print("Maximum total count for a word:")
    print(max(tot.counts))
    #print(head(count.word.doc.mat))
  }

  ## tot.word.counts <- colSums(count.word.doc.mat)
  ## kept.words <- which(tot.word.counts > 0)
  kept.words <- 1:V

  # Write out lda file
  write.lda.data(file.out=lda.filename,topics=topics,I.vecs=I.vecs,
                 count.word.doc.mat=count.word.doc.mat,
                 doc.lengths=doc.norm.lengths,doc.ids=doc.ids)

  return(kept.words)
  
}

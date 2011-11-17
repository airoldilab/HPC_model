# Scripts to get predicted topics for each topic and perform
# cross-validation exercise to get best delta

predict.topics <- function(delta,theta.param.vecs){

  topics <- colnames(theta.param.vecs)
  doc.ids <- rownames(theta.param.vecs)

  # Get predicted topics for each document
  # If no topics above delta, predict strongest topic loading
  predicted.doc.topics.list <- apply(theta.param.vecs,1,
                                     function(vec,delta,topics){
                                       pos.pred <- which(vec > delta)
                                       if(length(pos.pred)<1){
                                         pos.pred <- which.max(vec)}
                                       topics.pred <- topics[pos.pred]},
                                     delta=delta,topics=topics)

  names(predicted.doc.topics.list) <- doc.ids

  return(predicted.doc.topics.list)

}


topic.precision.recall <- function(predicted.doc.topics.list,
                                   doc.topic.list){

  # Get real topics for unseen documents
  predict.doc.ids <- names(predicted.doc.topics.list)
  relevant.doc.topic.list <- doc.topic.list[predict.doc.ids]
  
  # Get number of predictions for each document
  n.predict.topics <- sapply(predicted.doc.topics.list,length)

  # Get number of true topics for each document
  n.true.topics <- sapply(relevant.doc.topic.list,length)
  
  # Get number of correct predictions for each document
  predict.success <- mapply(function(pred.topics,real.topics)
                            {pred.topics %in% real.topics},
                            pred.topics=predicted.doc.topics.list,
                            real.topics=relevant.doc.topic.list)
  npredict.success <- sapply(predict.success,sum)

  # Get precision and recall measures
  precision <- sum(npredict.success)/sum(n.predict.topics)
  recall <- sum(npredict.success)/sum(n.true.topics)

  # Get F1 score (combined measure)
  f1 <- 2 * (precision * recall) / (precision + recall)

  out.list <- list(precision=precision,recall=recall,f1=f1)

  return(out.list)
}

# Function to find best delta using cross-validation
crossvalid.delta <- function(delta,theta.param.vecs,doc.topic.list){
  #print(delta)
  predicted.doc.topics.list <- predict.topics(delta=delta,theta.param.vecs)
  pr.success <- topic.precision.recall(predicted.doc.topics.list,
                                       doc.topic.list)
  f1 <- pr.success$f1
  return(f1)
}

# Function to find best delta using cross-validation
optim.delta <- function(theta.param.vecs,doc.topic.list,npoints=NULL,
                        optimize=FALSE,pdf.filename=NULL){

  # Use optimization if requested
  if(optimize){
    out.optim <- optimize(f=crossvalid.delta,interval=c(0,1),
                          maximum=TRUE,theta.param.vecs=theta.param.vecs,
                          doc.topic.list=doc.topic.list,
                          pdf.filename=NULL)
    delta.best <- out.optim$maximum
  }

  # Else use a simple grid search
  else{if(is.null(npoints)){stop("Must provide number of grid points if not using optimize")}
       ruler <- seq(0.01,0.99,length.out=npoints)
       cross.valid.f1 <- sapply(ruler,crossvalid.delta,
                                theta.param.vecs=theta.param.vecs,
                                doc.topic.list=doc.topic.list)
       pos.best <- which.max(cross.valid.f1)
       delta.best <- ruler[pos.best]

       # Create pdf of f1 curve if requested
       if(!is.null(pdf.filename)){
         pdf(pdf.filename)
         plot(ruler,cross.valid.f1,type="l")
         points(delta.best,crossvalid.delta(delta.best,theta.param.vecs,
                                            doc.topic.list))
         dev.off()
       }
  }

  return(delta.best)
  
}

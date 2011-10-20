# Functions to use MPI to update tree parameters

sprintft <- function(x,...){return(paste(date(),": ",sprintf(x,...),sep=""))}

inverse.hessian <- function(hessian){
  x <- svd(-hessian)
  inv.hessian <- x$u%*%diag(1/x$d)%*%x$v
  return(inv.hessian)
}

# Function the slaves will call to perform a validation on the
# fold equal to their slave number.
tree.slave.fn <- function(current.param.list,doc.length.vec,
                          feature.count.list,doc.topic.list,
                          topic.address.book,hessian=FALSE,
                          tree.slave.verbose=FALSE){
  
  # Note the use of the tag for sent messages: 
  #     1=ready_for_task, 2=done_task, 3=exiting, 9=shutdown
  # Note the use of the tag for received messages: 
  #     1=task, 2=done_tasks
  junk <- 0 
  done <- 0
  
  while(done != 1){
    # Signal being ready to receive a new task 
    mpi.send.Robj(obj=junk,dest=0,tag=1,comm=0) 

    # Receive a task 
    task <- mpi.recv.Robj(source=mpi.any.source(),tag=mpi.any.tag(),comm=0)
    task_info <- mpi.get.sourcetag() 
    tag <- task_info[2]
    
    if(tag == 1){ 
      # Get list of assigned tasks
      word.ids <- task

      # Create objects to hold results
      mu.f.res.mat <- c()
      mu.corpus.res.vec <- c()
      tau2.res.mat <- c()
      tree.conv <- c()
      tree.post <- c()
      if(hessian){tree.cov.list <- list()}
      
      # Records times of results
      times <- c()
      n.words <- length(word.ids)
      worker.id <- mpi.comm.rank(0)
      
      for(word.id in word.ids){

        # Time update
        t0 <-  proc.time()[3]
        
        # Update tree parameters
        out.optim.tree <- optim.tree(job.id=toString(word.id),
                                     current.param.list=current.param.list,
                                     doc.length.vec=doc.length.vec,
                                     doc.topic.list=doc.topic.list,
                                     feature.count.list=feature.count.list,
                                     topic.address.book=topic.address.book,
                                     corpus.topic="CORPUS",hessian=hessian)

        # Print time for each word
        t1 <- proc.time()[3]
        times <- c(times,t1-t0)

        if(tree.slave.verbose){
          word.pos <- which(word.ids==word.id)
          cat(sprintf("Worker %s finished word %s of %s (%s) in %f seconds\n",
                      worker.id,word.pos,n.words,word.id,t1-t0))
        }

        # Record values in results objects
        mu.f.res.mat <- rbind(mu.f.res.mat,out.optim.tree$mu.f)
        mu.corpus.res.vec[word.id] <- out.optim.tree$mu.0.f
        tau2.res.mat <- rbind(tau2.res.mat,out.optim.tree$tau2f.vec)
        tree.conv[word.id] <- out.optim.tree$global.conv
        tree.post[word.id] <- out.optim.tree$tree.post
        if(hessian){tree.cov.list[word.id] <- inverse.hessian(out.optim.tree$hessian.tree)}
        
        ## current.param.list$mu.param.vecs[word.id,] <- out.list[[word.id]]$mu.f
        ## current.param.list$mu.corpus.vec[word.id] <- out.list[[word.id]]$mu.0.f
        ## current.param.list$tau2.param.vecs[word.id,] <- out.list[[word.id]]$tau2f.vec
        ## tree.conv[word.id] <- out.list[[word.id]]$global.conv
        ## res.list[[word.id]] <- out.optim.tree
      }

      # Print overall times
      cat(sprintft("Slave %d finished %d jobs in an average of %f seconds with std dev of %f seconds\n",worker.id,n.words,mean(times),sd(times)))

      # Send results to master
      rownames(mu.f.res.mat) <- rownames(tau2.res.mat) <- word.ids
      res.list <- list(mu.f.res.mat=mu.f.res.mat,mu.corpus.res.vec=mu.corpus.res.vec,
                       tau2.res.mat=tau2.res.mat,tree.conv=tree.conv,tree.post=tree.post)
      mpi.send.Robj(obj=res.list,dest=0,tag=2,comm=0)

    } else if(tag == 2) {
      # If tag==2, shut down
      done <- 1

    } else {
      # Else if tag not recognized, raise error
      stop(paste("Tag", tag, "not recognized by slave"))}
  }

  # Tell master ready to shut down
  mpi.send.Robj(obj=junk,dest=0,tag=3,comm=0)

  # Wait for shutdown signal from master, then exit
  mpi.recv.Robj(source=0,tag=9,comm=0)

  # Return list of covariance matrices if Hessian requested
  if(hessian){return(tree.cov.list)}
}


# Function for master node
tree.master.fn <- function(tree.job.list,current.param.list){

  # Create convergence marker list
  tree.conv <- c()
  tree.post <- c()
 
  # Create list to hold results
  res.list <- list()

  junk <- 0 
  closed.slaves <- 0 
  n.slaves <- mpi.comm.size(comm=0)-1

  while(closed.slaves < n.slaves){ 
    # Receive a res.list from a slave 
    res.list <- mpi.recv.Robj(source=mpi.any.source(),tag=mpi.any.tag(),comm=0)
    res.list.info <- mpi.get.sourcetag() 
    slave.id <- res.list.info[1]
    slave.id.str <- toString(slave.id)
    tag <- res.list.info[2] 

    if(tag == 1){ 
      # Give the slave his task or tell him he is done
      # are done if there are none. 
      #if(length(tree.job.list) > 0){
      if(!is.null(tree.job.list[[slave.id.str]])){
        # Send his task, and then remove it from the task list 
        slave.task <- tree.job.list[[slave.id.str]]
        mpi.send.Robj(obj=slave.task,dest=slave.id,tag=1,comm=0)
        tree.job.list[[slave.id.str]] <- NULL 
      } else {
        # Send message to shut down slave
        mpi.send.Robj(obj=junk,dest=slave.id,tag=2,comm=0)}
      
    } else if(tag == 2) { 
      # The res.list contains the updated tree parameters and the
      # global convergence markers
      word.ids.job <- names(res.list$tree.conv)
      current.param.list$mu.param.vecs[word.ids.job,] <- res.list$mu.f.res.mat
      current.param.list$mu.corpus.vec[word.ids.job] <- res.list$mu.corpus.res.vec
      current.param.list$tau2.param.vecs[word.ids.job,] <- res.list$tau2.res.mat
      tree.conv <- c(tree.conv,res.list$tree.conv)
      tree.post <- c(tree.post,res.list$tree.post)
      
    } else if(tag == 3) { 
       # A slave has closed down
      closed.slaves <- closed.slaves + 1
 
    } else {
      # Else if tag not recognized, raise error
      stop(paste("Tag", tag, "not recognized by master"))}
    }

  out.list <- list(current.param.list=current.param.list,tree.conv=tree.conv,
                   tree.post=tree.post)

  # Send shutdown message to slaves
  for(i in 1:n.slaves){mpi.send.Robj(obj=junk,dest=i,tag=9,comm=0)}

  return(out.list)
}




  ## # Divide list of tasks into blocks of mpi.block.size chunks
  ## # How many blocks
  ## nwords <- length(word.ids)
  ## remainder <- nwords %% mpi.block.size > 0
  ## njobs <- trunc(nwords/mpi.block.size) + ifelse(remainder,1,0)

  ## # Create job list
  ## tree.job.list <- list()
  ## for(pos in 1:njobs){
  ##   pos.do <- c(1:mpi.block.size)+(pos-1)*mpi.block.size
  ##   pos.do <- pos.do[pos.do <= nwords]
  ##   tree.job.list[[pos]] <- word.ids[pos.do]
  ## }     


      ## word.ids.job <- names(res.list)
      ## for(word.id in word.ids.job){
      ##   current.param.list$mu.param.vecs[word.id,] <- res.list[[word.id]]$mu.f
      ##   current.param.list$mu.corpus.vec[word.id] <- res.list[[word.id]]$mu.0.f
      ##   current.param.list$tau2.param.vecs[word.id,] <- res.list[[word.id]]$tau2f.vec
      ##   tree.conv[word.id] <- res.list[[word.id]]$global.conv

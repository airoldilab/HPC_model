# Functions to use MPI to sample xis

sprintft <- function(x,...){return(paste(date(),": ",sprintf(x,...),sep=""))}

# Function the slaves will call to perform a validation on the
# fold equal to their slave number
xi.slave.fn <- function(current.param.list,doc.length.vec,
                        doc.count.list,doc.topic.list,
                        topic.address.book,
                        hessian.like.list=NULL,
                        xi.slave.verbose=FALSE,
                        classify=FALSE){
  
  # Note the use of the tag for sent messages: 
  #     1=ready_for_task, 2=done_task, 3=exiting, 9=shutdown 
  # Note the use of the tag for received messages: 
  #     1=task, 2=done_tasks
  junk <- 0 
  done <- 0
  worker.id <- mpi.comm.rank(0)

  # Don't return a hessian unless get tag 0
  return.hessian <- FALSE
  
  while(done != 1){
    # Signal being ready to receive a new task
    mpi.send.Robj(obj=junk,dest=0,tag=1,comm=0) 
    
    # Receive a task
    
    task <- mpi.recv.Robj(source=mpi.any.source(),tag=mpi.any.tag(),comm=0)
    #cat(paste("Slave",worker.id,"got a task\n"))
    task_info <- mpi.get.sourcetag() 
    tag <- task_info[2]

    # For tag==0, maximize the conditional posterior and refresh
    # our estimate of the Hessian of the likelihood
    if(tag == 0){

      # Need to return hessian list
      return.hessian <- TRUE

      worker.id <- mpi.comm.rank(0)
      cat(sprintft("Worker %s refreshing likelihood hessian for doc jobs\n",
                  worker.id))
      
      # Get list of assigned tasks
      doc.ids <- task

      # Create list to hold likelihood hessians
      hessian.like.list <- list()

      for(doc.id in doc.ids){
        
        # Time update
        t0 <-  proc.time()[3]

        # Get previous draw
        xi.old <- current.param.list$xi.param.vecs[doc.id,]

        # Get mode of xi parameters
        optim.xi.out <- optim.xi(job.id=doc.id,
                                 current.param.list=current.param.list,
                                 doc.length.vec=doc.length.vec,
                                 doc.count.list=doc.count.list,
                                 doc.topic.list=doc.topic.list,
                                 xi.data.out=TRUE,hessian=TRUE,
                                 active.only=FALSE)
        
        # Get hessian of likelihood at mode
        xi.data.list <- optim.xi.out$xi.data.list
        optim.hes <- optim.xi.out$hessian
        hessian.like.list[[doc.id]] <- optim.hes + xi.data.list$Sigma.inv

      }}

    # For tag is 0 or 1, get a new draw from the conditional posterior
    if(any(tag == 0,tag == 1)){ 
      # Get list of assigned tasks
      doc.ids <- task

      ## # For testing purposes, only do first few docs
      ## doc.ids <- doc.ids[1:5]

      # Need to grab parameters to be updated
      # Create objects to hold results
      xi.res.mat <- current.param.list$xi.param.vecs[doc.ids,]
      theta.res.mat <- current.param.list$theta.param.vecs[doc.ids,]

      # Records times of results
      times <- c()
      n.docs <- length(doc.ids)
      worker.id <- mpi.comm.rank(0)
      
      for(doc.id in doc.ids){
        
        # Time update
        t0 <-  proc.time()[3]

        # Get previous draw
        xi.old <- current.param.list$xi.param.vecs[doc.id,]
        
        # Get new xi draw
        #cat(paste("Slave",worker.id,"optimizing doc",doc.id,"\n"))
        hmc.draws <- hmc.xi(job.id=doc.id,ndraws=1,step.size=0.2,nsteps=10,
                            current.param.list=current.param.list,
                            doc.length.vec=doc.length.vec,
                            doc.topic.list=doc.topic.list,
                            doc.count.list=doc.count.list,
                            hessian.like=hessian.like.list[[doc.id]],
                            active.only=FALSE,last.draw=TRUE,
                            Nfeat.case.control=NULL)
                            # Eventually need 'classify' option
        
        xi.draw <- hmc.draws
        xi.res.mat[doc.id,] <- xi.draw
        active.topics <- doc.topic.list[[doc.id]]
        # Update relevant entry of theta.res.mat as a function of new xis
        xi.vec.active <- xi.draw[active.topics]
        theta.res.mat[doc.id,active.topics] <- get.theta.from.xi(xi.vec.active)
        ## theta.res.mat[doc.id,active.topics] <-
        ##   exp(xi.draw[active.topics])/sum(exp(xi.draw[active.topics]))

        # Record time
        t1 <- proc.time()[3]
        times <- c(times,t1-t0)
        
        if(any(classify,xi.slave.verbose)){
          doc.pos <- which(doc.ids==doc.id)
          cat(sprintft("Worker %s finished doc %s of %s (%s) in %f seconds\n",
                     worker.id,doc.pos,n.docs,doc.id,t1-t0))}

      }

      # Print times
      cat(sprintft("Slave %d finished %d jobs in an average of %f seconds with std dev of %f seconds\n",
                   worker.id,n.docs,mean(times),sd(times)))

      # Send results to master
      res.list <- list(xi.res.mat=xi.res.mat,theta.res.mat=theta.res.mat)
      ## cat(sprintft("Slave %d is sending results to master\n",worker.id))
      mpi.send.Robj(obj=res.list,dest=0,tag=2,comm=0)

    # If tag==2, shut down
    } else if(tag == 2) {     
      ## cat(sprintft("Slave %d got 'done' signal from master\n",worker.id))
      done <- 1

    } else {
      # Else if tag not recognized, raise error
      stop(paste("Tag", tag, "not recognized by slave"))}
  }

  ## cat(sprintft("Slave %d is telling master ready to leave xi function\n",
  ##     worker.id))
  
  # Tell master ready to shut down
  mpi.send.Robj(obj=junk,dest=0,tag=3,comm=0)
  
  ## cat(sprintft("Slave %d is waiting for shutdown signal in xi fn\n",
  ##     worker.id))
  
  # Wait for shutdown signal from master, then exit
  task <- mpi.recv.Robj(source=0,tag=9,comm=0)

  ## cat(sprintft("Slave %d is leaving the xi fn\n",worker.id))
  
  # Return hessian.like.list if a refresh iteration
  out.list <- list(return.hessian=return.hessian)
  if(return.hessian){out.list$hessian.like.list <- hessian.like.list}

  return(out.list)
}


# Function for master node
xi.master.fn <- function(xi.job.list,xi.param.vecs,theta.param.vecs,
                         classify=FALSE,update.hessian.like=FALSE){

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
    
    if(tag == 1) {
      # Give the slave his task or tell him he is done
      if(!is.null(xi.job.list[[slave.id.str]])){
        # Send his task, and then remove it from the task list 
        slave.task <- xi.job.list[[slave.id.str]]
        tag.send <- ifelse(update.hessian.like,0,1)
        mpi.send.Robj(obj=slave.task,dest=slave.id,tag=tag.send,comm=0)
        xi.job.list[[slave.id.str]] <- NULL 
      } else {
        # Send message to shut down slave
        ## cat(sprintft("Master is telling slave %d to leave xi function\n",
      ## slave.id))
        mpi.send.Robj(obj=junk,dest=slave.id,tag=2,comm=0)
      }
      
    } else if(tag == 2) { 
      # The res.list contains the updated xi parameters and the
      # global convergence markers
      ## cat(sprintft("Master is processing data from slave %d\n",
      ## slave.id))
      doc.ids.job <- rownames(res.list$xi.res.mat)
      xi.param.vecs[doc.ids.job,] <- res.list$xi.res.mat
      theta.param.vecs[doc.ids.job,] <- res.list$theta.res.mat      
      #print(head(xi.param.vecs))
      
    } else if(tag == 3) { 
       # A slave has closed down. 
      closed.slaves <- closed.slaves + 1
      ## cat(sprintft("Master has closed %d slaves\n",closed.slaves))
      
      } else {
      # Else if tag not recognized, raise error
      stop(paste("Tag", tag, "not recognized by master"))}
    }

  ## cat(sprintft("Master has received all xi jobs and is creating output list\n"))
  # Need to return implied thetas as well though
  out.list <- list(xi.param.vecs=xi.param.vecs,
                   theta.param.vecs=theta.param.vecs)

  # Send shutdown message to slaves
  for(i in 1:n.slaves){mpi.send.Robj(obj=junk,dest=i,tag=9,comm=0)}

  ## cat(sprintft("Master in exiting xi master fucntion\n"))
  
  return(out.list)
}






 ## # Divide list of tasks into blocks of mpi.block.size chunks
  ## # How many blocks
  ## ndocs <- length(doc.ids)
  ## remainder <- ndocs %% mpi.block.size > 0
  ## njobs <- trunc(ndocs/mpi.block.size) + ifelse(remainder,1,0)

  ## # Create job list
  ## theta.job.list <- list()
  ## for(pos in 1:njobs){
  ##   pos.do <- c(1:mpi.block.size)+(pos-1)*mpi.block.size
  ##   pos.do <- pos.do[pos.do <= ndocs]
  ##   theta.job.list[[pos]] <- doc.ids[pos.do]
  ## }

      ## doc.ids.job <- names(out.list)
      ## for(doc.id in doc.ids.job){
      ##   theta.param.vecs[doc.id,] <- out.list[[doc.id]]$theta.d
      ##   doc.conv[doc.id] <- out.list[[doc.id]]$global.conv
      ## } 

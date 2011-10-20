# Functions to use MPI to update thetas

sprintft <- function(x,...){return(paste(date(),": ",sprintf(x,...),sep=""))}

# Function the slaves will call to perform a validation on the
# fold equal to their slave number.
# Assumes: thedata,fold,foldNumber,p
theta.slave.fn <- function(current.param.list,doc.length.vec,
                           doc.count.list,doc.topic.list,
                           topic.address.book,classify=FALSE){
  
  # Note the use of the tag for sent messages: 
  #     1=ready_for_task, 2=done_task, 3=exiting, 9=shutdown 
  # Note the use of the tag for received messages: 
  #     1=task, 2=done_tasks
  junk <- 0 
  done <- 0
  worker.id <- mpi.comm.rank(0)
  
  while(done != 1){
    # Signal being ready to receive a new task
    mpi.send.Robj(obj=junk,dest=0,tag=1,comm=0) 
    
    # Receive a task
    
    task <- mpi.recv.Robj(source=mpi.any.source(),tag=mpi.any.tag(),comm=0)
    #cat(paste("Slave",worker.id,"got a task\n"))
    task_info <- mpi.get.sourcetag() 
    tag <- task_info[2]
    
    if(tag == 1) { 
      # Get list of assigned tasks
      doc.ids <- task

      ## # For testing purposes, only do first few docs
      ## doc.ids <- doc.ids[1:5]
      
      # Create objects to hold results
      theta.res.mat <- c()
      doc.conv <- c()

      # Records times of results
      times <- c()
      n.docs <- length(doc.ids)
      
      for(doc.id in doc.ids){
      
        
        # Time update
        t0 <-  proc.time()[3]
        
        # Update theta
        #cat(paste("Slave",worker.id,"optimizing doc",doc.id,"\n"))
        out.optim.theta <- optim.theta(job.id=doc.id,
                                       current.param.list=current.param.list,
                                       doc.length.vec=doc.length.vec,
                                       doc.count.list=doc.count.list,
                                       topic.address.book=topic.address.book,
                                       doc.topic.list=doc.topic.list,
                                       debug=FALSE,enforce.constr=FALSE,
                                       classify=classify)
        

        theta.res.mat <- rbind(theta.res.mat,out.optim.theta$theta.d)
        doc.conv[doc.id] <- out.optim.theta$global.conv

        ## # Print time
        doc.pos <- which(doc.ids==doc.id)
        t1 <- proc.time()[3]
        times <- c(times,t1-t0)
        #cat(paste("Slave",worker.id,"optimized doc",doc.id,
        #          "in",t1-t0,"seconds\n"))
        if(classify){cat(sprintft("Worker %s finished doc %s of %s (%s) in %f seconds\n",
                     worker.id,doc.pos,n.docs,doc.id,t1-t0))}
        
        ## # Record value in res.list
        ## res.list[[doc.id]] <- out.optim.theta
      }

      # Print times
      cat(sprintft("Slave %d finished %d jobs in an average of %f seconds with std dev of %f seconds\n",worker.id,n.docs,mean(times),sd(times)))

      # Send results to master
      rownames(theta.res.mat) <- doc.ids
      res.list <- list(theta.res.mat=as.matrix(theta.res.mat),
                       doc.conv=doc.conv)
      #print(res.list)
      ## cat(sprintft("Slave %d is sending results to master\n",worker.id))
      mpi.send.Robj(obj=res.list,dest=0,tag=2,comm=0)
      
    } else if(tag == 2) {
      # If tag==2, shut down
      ## cat(sprintft("Slave %d got 'done' signal from master\n",worker.id))
      done <- 1

    } else {
      # Else if tag not recognized, raise error
      stop(paste("Tag", tag, "not recognized by slave"))}
  }

  ## cat(sprintft("Slave %d is telling master ready to leave theta function\n",
  ##     worker.id))
  
  # Tell master ready to shut down
  mpi.send.Robj(obj=junk,dest=0,tag=3,comm=0)
  
  ## cat(sprintft("Slave %d is waiting for shutdown signal in theta fn\n",
  ##     worker.id))
  
  # Wait for shutdown signal from master, then exit
  task <- mpi.recv.Robj(source=0,tag=9,comm=0)

  ## cat(sprintft("Slave %d is leaving the theta fn\n",worker.id))
}


# Function for master node
theta.master.fn <- function(theta.job.list,theta.param.vecs,classify=FALSE){

  # Create convergence marker list
  doc.conv <- c()

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
      #if(length(theta.job.list) > 0){
      if(!is.null(theta.job.list[[slave.id.str]])){
        #print(length(theta.job.list))
        # Send his task, and then remove it from the task list 
        slave.task <- theta.job.list[[slave.id.str]]
        mpi.send.Robj(obj=slave.task,dest=slave.id,tag=1,comm=0)
        theta.job.list[[slave.id.str]] <- NULL 
      } else {
        # Send message to shut down slave
        ## cat(sprintft("Master is telling slave %d to leave theta function\n",
      ## slave.id))
        mpi.send.Robj(obj=junk,dest=slave.id,tag=2,comm=0)
      }
      
    } else if(tag == 2) { 
      # The res.list contains the updated theta parameters and the
      # global convergence markers
      ## cat(sprintft("Master is processing data from slave %d\n",
      ## slave.id))
      #print(res.list$theta.res.mat)
      doc.ids.job <- rownames(res.list$theta.res.mat)
      theta.param.vecs[doc.ids.job,] <- res.list$theta.res.mat
      #print(head(theta.param.vecs))
      doc.conv <- c(doc.conv,res.list$doc.conv)
      
    } else if(tag == 3) { 
       # A slave has closed down. 
      closed.slaves <- closed.slaves + 1
      ## cat(sprintft("Master has closed %d slaves\n",closed.slaves))
      
      } else {
      # Else if tag not recognized, raise error
      stop(paste("Tag", tag, "not recognized by master"))}
    }

  ## cat(sprintft("Master has received all theta jobs and is creating output list\n"))
  out.list <- list(doc.conv=doc.conv,
                   theta.param.vecs=theta.param.vecs)

  # Send shutdown message to slaves
  for(i in 1:n.slaves){mpi.send.Robj(obj=junk,dest=i,tag=9,comm=0)}

  ## cat(sprintft("Master in exiting theta master fucntion\n"))
  
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

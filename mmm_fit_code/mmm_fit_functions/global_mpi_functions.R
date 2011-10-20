# Functions to coordinate MPI at the global level

mpi.slave.fn <- function(topic.address.book,
                         file.current.param.list,
                         slave.file.root="slave_data"){

  # Note the use of the tag for sent messages: 
  # 4=ready_for_task,
  # Note the use of the tag for received messages: 
  # 5=tree update, 6=theta update, 7=exit

  # Junk message
  junk <- 0
  # Flag for exiting function
  exit <- 0

  # Wait for signal to load up data
  slave.id <- mpi.comm.rank(0)
  mpi.recv.Robj(source=0,tag=88,comm=0)
  file.data <- paste(slave.file.root,slave.id,".RData",sep="")
  load(file.data)
  #cat(sprintf("\nNode %s\n",toString(mpi.comm.rank(0))))
  #print(gc())

  while(exit != 1){
    # Signal being ready to receive a new command
    mpi.send.Robj(obj=junk,dest=0,tag=4,comm=0) 

    # Receive a task
    task <- mpi.recv.Robj(source=mpi.any.source(),tag=mpi.any.tag(),comm=0)
    task_info <- mpi.get.sourcetag()
    #print(paste("task_info:",task_info))
    tag <- task_info[2]

    # Now do one of four things: update tree parameters, update theta vector,
    # signal ready to shut down, or exit
    if(tag == 5){
      # Load in fresh param list
      load(file.current.param.list)
      # Update parameters requested
      if(!exists("mu.hessian.like.list")){mu.hessian.like.list <- NULL}
      out.tree <- tree.slave.fn(current.param.list=current.param.list,
                                doc.length.vec=doc.length.vec,
                                feature.count.list=feature.count.list,
                                doc.topic.list=doc.topic.list,
                                topic.address.book=topic.address.book,
                                hessian.like.list=mu.hessian.like.list)
      if(out.tree$return.hessian){
        mu.hessian.like.list <- out.tree$hessian.like.list}
    }

    else if(tag == 6){
      # Load in fresh param list
      load(file.current.param.list)
      # Update parameters requested
      if(!exists("xi.hessian.like.list")){xi.hessian.like.list <- NULL}
      out.xi <- xi.slave.fn(current.param.list=current.param.list,
                            doc.length.vec=doc.length.vec,
                            doc.count.list=doc.count.list,
                            doc.topic.list=doc.topic.list,
                            topic.address.book=topic.address.book,
                            hessian.like.list=xi.hessian.like.list)
      if(out.xi$return.hessian){
        xi.hessian.like.list <- out.xi$hessian.like.list}
    }

    # If tag==7, shut down
    else if(tag == 7){exit <- 1}

    # Else if tag not recognized, raise error
    else{stop(paste("Tag", tag, "not recognized by slave"))}
  }

  # Tell master exiting
  mpi.send.Robj(obj=junk,dest=0,tag=7,comm=0)

}


# Function to master nodes to tell slaves which parameters to update
master.param.fn <- function(param.tag){

  junk <- 0 
  n.slaves <- mpi.comm.size(comm=0)-1
  # Number of slaves in correct state
  guided.slaves <- 0
  
  while(guided.slaves < n.slaves){
    # Receive status update from a slave---only want tag 4
    status <- mpi.recv.Robj(source=mpi.any.source(),tag=4,comm=0)
    status.info <- mpi.get.sourcetag() 
    slave.id <- status.info[1] 
    tag <- status.info[2]

    # Since slave in ready state, tell to prepare for parameter update
    mpi.send.Robj(obj=junk,dest=slave.id,tag=param.tag,comm=0)
    guided.slaves <- guided.slaves + 1
  }

}



# Function for master node to tell slaves to exit
master.exit.fn <- function(){

  junk <- 0 
  exited.slaves <- 0 
  n.slaves <- mpi.comm.size(comm=0)-1
  
  while(exited.slaves < n.slaves){ 
    # Receive status update from a slave 
    status <- mpi.recv.Robj(source=mpi.any.source(),tag=mpi.any.tag(),comm=0)
    status.info <- mpi.get.sourcetag() 
    slave.id <- status.info[1] 
    tag <- status.info[2]

    # If slave still in ready state, tell to shut down
    if(tag == 4){
      mpi.send.Robj(obj=junk,dest=slave.id,tag=7,comm=0)
    }

    # If slave has exited, increase exit count
    else if(tag == 7){ 
      # A slave has exited
      exited.slaves <- exited.slaves + 1 
    }

    # Else if tag not recognized, raise error
    else{stop(paste("Tag", tag, "not recognized by master.exit"))}
  }

}



## # If tag==1, worker node somehow got stuck in the one of the
##     # update functions and needs to be guided out before assigning
##     # new task
##     else if(tag == 1){
##       # Send the slave message to shut down
##       mpi.send.Robj(obj=junk,dest=slave.id,tag=2,comm=0)
##       # Wait for confirmation from slave
##       mpi.recv.Robj(source=slave.id,tag=3,comm=0)
##       # Send slave shutdown msg
##       mpi.send.Robj(obj=junk,dest=slave.id,tag=9,comm=0)
##       # Wait for ready message from slave
##       mpi.recv.Robj(source=slave.id,tag=4,comm=0)
##       # Now give slave correct instruction
##       mpi.send.Robj(obj=junk,dest=slave.id,tag=param.tag,comm=0)
##     }


## # Function to master nodes to tell slaves which parameters to update
## master.param.fn <- function(param.tag){

##   junk <- 0 
##   n.slaves <- mpi.comm.size(comm=0)-1
##   # Number of slaves in correct state
##   guided.slaves <- 0
  
##   while(guided.slaves < n.slaves){
##     # Receive status update from a slave 
##     status <- mpi.recv.Robj(source=mpi.any.source(),tag=mpi.any.tag(),comm=0)
##     status.info <- mpi.get.sourcetag() 
##     slave.id <- status.info[1] 
##     tag <- status.info[2]

##     # If slave still in ready state, tell to prepare for parameter update
##     if(tag == 4){
##       mpi.send.Robj(obj=junk,dest=slave.id,tag=param.tag,comm=0)
##       guided.slaves <- guided.slaves + 1
##     }

##     # If slave has early, raise error
##     else if(tag == 7){stop("Slave exited early!")}

##     # If tag==1, already saw this worker---get it back in ready position
##     else if(tag == 1){}

##     # Else if tag not recognized, raise error
##     else{stop(paste("Tag", tag, "not recognized by master.param"))}
##   }

## }

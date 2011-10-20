# Global coordinate ascent algorithm

library(Matrix)

global.coor.ascent <- function(current.param.list,
                               feature.count.list=NULL,
                               doc.count.list=NULL,
                               topic.address.book=NULL,
                               max.iter=1000,
                               verbose=FALSE,
                               print.iter=FALSE,
                               print.treeiter=FALSE,
                               print.thetaiter=FALSE,
                               print.conv=FALSE,
                               debug=FALSE,use.mpi=FALSE,
                               tree.job.list=NULL,
                               theta.job.list=NULL,
                               file.current.param.list=NULL,
                               post.conv=TRUE,
                               file.post.conv=NULL){


  # If using MPI, send out signal to slaves to load up their data
  if(use.mpi){
    n.slaves <- mpi.comm.size(0)
    for(slave.id in 1:n.slaves){mpi.send.Robj(obj=0,dest=slave.id,tag=88,comm=0)}
  }

  # If not using MPI, need to set up indicies to cycle through
  if(!use.mpi){
    # Get indexes to cycle through
    word.ids <- names(feature.count.list)
    doc.ids <- names(doc.count.list)

    # Figure out which theta.param.vecs need to be updated (have more than
    # one active topic)
    active.docs <- apply(current.param.list$theta.param.vecs,1,
                         function(vec){length(which(vec>0))>1})
    # Only update active docs
    doc.ids <- doc.ids[active.docs]

    # Global convergence indicators
    tree.conv <- rep(FALSE,length(word.ids))
    names(tree.conv) <- word.ids
    doc.conv <- rep(FALSE,length(doc.ids))
    names(doc.conv) <- doc.ids

    # Initialize hyperameters
    kappa.nu <- 5
    lambda.nu <- 5/kappa.nu
    kappa.sigma2 <- 5
    lambda.sigma2 <- 0.05/kappa.sigma2
    hparam.outlist <- update.hparam(current.param.list=current.param.list,
                                    kappa.nu=kappa.nu,lambda.nu=lambda.nu,
                                    kappa.sigma2=kappa.sigma2,
                                    lambda.sigma2=lambda.sigma2)
    current.param.list <- hparam.outlist$current.param.list
  }
  

  if(use.mpi){
    # Write out initial current.param.list
    save(current.param.list,file=file.current.param.list)
    #cat(sprintf("\nNode %s\n",toString(mpi.comm.rank(0))))
    #print(gc())
}

  # Cycle through independent sets of updates
  joint.post.vec <- c()
  for(i in 1:max.iter){
    if(print.iter){cat(paste("\nGlobal iteration",i,"\n"))}

    #if(i>1){
    # Cycle through all words and update tree parameters
    if(use.mpi){
      # Update tree parameters using MPI
      # Tell slave nodes to prepare to update tree parameters
      cat("Tree update\n")
      master.param.fn(param.tag=5)
      tree.master.out <- tree.master.fn(tree.job.list=tree.job.list,
                                        current.param.list=current.param.list)
      current.param.list <- tree.master.out$current.param.list
      tree.conv <- tree.master.out$tree.conv

      if(post.conv){
        tree.post <- tree.master.out$tree.post
        #print(tree.post)
        joint.post <- sum(tree.post)
        if(i > 1){joint.post.old <- joint.post.vec[length(joint.post.vec)]}
        joint.post.vec <- c(joint.post.vec,joint.post)
        if(i > 1) {global.conv <- check.conv(joint.post.old,joint.post,
                                             reltol=1e-5)}
        if(!is.null(file.post.conv)){
          write.table(joint.post.vec,file=file.post.conv,col.names=FALSE,
                      quote=FALSE)}
      }

      # Write out new current.param.list
      save(current.param.list,file=file.current.param.list)
    }
  #}

    #if(i==1){tree.conv=FALSE}

    
    if(!use.mpi){
      for(word.id in word.ids){
        # Update tree parameters
        if(print.treeiter){print(paste("Tree update for word",word.id))}
        out.optim.tree <- optim.tree(job.id=toString(word.id),
                                     current.param.list=current.param.list,
                                     doc.length.vec=doc.length.vec,
                                     doc.topic.list=doc.topic.list,
                                     count.list=feature.count.list,
                                     topic.address.book=topic.address.book,
                                     corpus.topic="CORPUS")
        
        # Extract updated parameters
        mu.f.vec <- out.optim.tree$mu.f
        mu.0.f <- out.optim.tree$mu.0.f
        tau2f.vec <- out.optim.tree$tau2f.vec
        tree.global.conv <- out.optim.tree$global.conv

        # Update current values
        current.param.list$mu.param.vecs[word.id,] <- mu.f.vec
        current.param.list$mu.corpus.vec[word.id] <- mu.0.f
        current.param.list$tau2.param.vecs[word.id,] <- tau2f.vec
        tree.conv[word.id] <- tree.global.conv
      }
    }

    # Update hyperameters
    kappa.nu <- 5
    lambda.nu <- 5/kappa.nu
    kappa.sigma2 <- 5
    lambda.sigma2 <- 0.05/kappa.sigma2
    hparam.outlist <- update.hparam(current.param.list=current.param.list,
                                    kappa.nu=kappa.nu,lambda.nu=lambda.nu,
                                    kappa.sigma2=kappa.sigma2,
                                    lambda.sigma2=lambda.sigma2)
    current.param.list <- hparam.outlist$current.param.list
    hparam.conv <- hparam.outlist$global.conv

    # Cycle through all docs and update membership parameters
    if(use.mpi){
      # Update thetas using MPI
      # Tell slave nodes to prepare to update thetas
      cat("Theta update\n")
      master.param.fn(param.tag=6)
      theta.master.out <- theta.master.fn(theta.job.list=theta.job.list,
                                          theta.param.vecs=
                                          current.param.list$theta.param.vecs)
      current.param.list$theta.param.vecs <- as(theta.master.out$theta.param.vecs,
                                                "sparseMatrix")
      doc.conv <- theta.master.out$doc.conv

      # Write out new current.param.list
      save(current.param.list,file=file.current.param.list)
    }

    
    if(!use.mpi){
      for(doc.id in doc.ids){
        if(print.thetaiter){print(paste("Theta update for doc",doc.id))}
      
        # Update theta
        out.optim.theta <- optim.theta(job.id=toString(doc.id),
                                       current.param.list=current.param.list,
                                       doc.length.vec=doc.length.vec,
                                       doc.topic.list=doc.topic.list,
                                       doc.count.list=doc.count.list,
                                       topic.address.book=topic.address.book,
                                       debug=debug,enforce.constr=FALSE)

        # Extract updated parameters
        theta.d <- out.optim.theta$theta.d
        doc.global.conv <- out.optim.theta$global.conv

        # Update current values
        current.param.list$theta.param.vecs[doc.id,] <- theta.d
        doc.conv[doc.id] <- doc.global.conv
      }
      # Maintain sparse matrix representation
      current.param.list$theta.param.vecs <- as(current.param.list$theta.param.vecs,"sparseMatrix")
    }

    # Check for global convergence of all parameters
    if(print.conv){
      print(paste("joint.post:",joint.post))
      print("tree.conv")
      print(c(sum(tree.conv),length(tree.conv)))
      #print(which(tree.conv==FALSE))
      print("doc.conv")
      print(c(sum(doc.conv),length(doc.conv)))
      #print(which(doc.conv==FALSE))
      print("hparam.conv")
      print(hparam.conv)
      hparam.vec <- c(current.param.list$psi,current.param.list$gamma,
                      current.param.list$nu,current.param.list$sigma2)
      names(hparam.vec) <- c("psi","gamma","nu","sigma2")
      print(hparam.vec)
  }
    if(i > 1){if(post.conv & global.conv){break}}
    if(!post.conv){if(all(tree.conv,doc.conv,hparam.conv)){break}}
  }

  # Tell worker nodes to exit
  master.exit.fn()

  # Print convergence info if requested
  if(verbose){
    if(i<max.iter){print(paste("Converged in", i, "iterations"))}
    if(i==max.iter){
      print(paste("Failed to converge within", max.iter, "iterations"))}
  }

  # Return final set of parameters
  return(current.param.list)
}






  ## # If using MPI and request "best" blocksize, calibrate here
  ## if(all(use.mpi,any(mpi.tree.block.size=="best",mpi.theta.block.size=="best"))){
  ##   n.slaves <- mpi.comm.size(comm=0)-1
  ##   if(mpi.tree.block.size=="best"){
  ##     n.words <- length(word.ids)
  ##     mpi.tree.block.size <- ceiling(n.words/n.slaves)
  ##   }
  ##   if(mpi.theta.block.size=="best"){
  ##     n.docs <- length(doc.ids)
  ##     mpi.theta.block.size <- ceiling(n.docs/n.slaves)
  ##   }
  ## }

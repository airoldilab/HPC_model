# Function to draw hyperparameters for HPD model

hparam.draw <- function(current.param.list,tree.update=TRUE,
                        xi.update=TRUE,nsteps.tau2=5){

  out.list <- list()
  D <- current.param.list$D
  K <- current.param.list$K
  V <- current.param.list$V
  
  if(tree.update){
    # Draw mu hyperparameters
    mu.corpus.vec <- current.param.list$mu.corpus.vec
    gamma2.old <- (current.param.list$gamma)^2
    tree.out <- tree.hparam.draw(mu.corpus.vec=mu.corpus.vec,
                                 gamma2.old=gamma2.old,V=V)
    psi.new <- tree.out$psi
    gamma.new <- tree.out$gamma

    # Draw tau2 hyperparameters
    ## nu.old <- current.param.list$nu
    ## sigma2.old <- current.param.list$sigma2
    ## # Get old draws on gamma scale
    ## par.old <- convert.hparams(par=c(nu.old,sigma2.old),to.invchisq=FALSE)
    ## metro.out <- metro.sampler(n=nsteps.tau2,
    ##                            import.sampler=gamma.import.sampler,
    ##                            current.param.list=current.param.list,
    ##                            par.start=par.old,last.draw=TRUE)
    ## draw.invchisq <- metro.out$draws
    ## nu.new <- draw.invchisq[1]
    ## sigma2.new <- draw.invchisq[2]
    hmc.out <- hmc.gamma(ndraws=10,step.size=0.25,nsteps=20,
                         current.param.list=current.param.list,
                         debug=FALSE,last.draw=TRUE)
    nu.new <- hmc.out[1]
    sigma2.new <- hmc.out[2]

    # Save draws in output list
    out.list$psi <- psi.new
    out.list$gamma <- gamma.new
    out.list$nu <- nu.new
    out.list$sigma2 <- sigma2.new
  }

  if(xi.update){
    lambda2.old <- current.param.list$lambda2
    xi.out <- xi.hparam.draw(xi.param.vecs=current.param.list$xi.param.vecs,
                             lambda2.old=lambda2.old,D=D,K=K)
    eta.vec.new <- xi.out$eta.vec
    lambda2.new <- xi.out$lambda2
    out.list$eta.vec <- eta.vec.new
    out.list$lambda2 <- lambda2.new
  }

  return(out.list)
}

# Get draws from mixed membership model

library(MCMCpack)
library(geoR)
library(mvtnorm)

# Define functions

# Functions to get theta vecs
gen.theta.param.vecs <- function(D,eta.vec=NULL,Sigma=NULL,alpha=NULL,gp="dirichlet",verbose=FALSE){
  if(gp=="dirichlet"){
    xi.vecs <- rdirichlet(D,alpha)
    I.vecs <- t(apply(xi.vecs,1,gen.I.vec,delta=0.1,gp=gp))
  } else if(any(gp=="logit.norm",gp=="mv.probit")){
    xi.vecs <- rmvnorm(n=D,mean=eta.vec,sigma=Sigma)
    I.vecs <- t(apply(xi.vecs,1,gen.I.vec,gp=gp))
  }
  
  if(verbose){
    print("Topic distribution:")
    print(table(apply(I.vecs,1,sum)))
  }
  theta.param.vecs <- t(mapply(gen.theta.vec,xi.vec=as.data.frame(t(xi.vecs)),
                       I.vec=as.data.frame(t(I.vecs)),MoreArgs=list(gp=gp)))
  return(list(theta.param.vecs=theta.param.vecs,I.vecs=I.vecs,xi.vecs=xi.vecs))
}

gen.I.vec <- function(xi.vec,gp="dirichlet",delta=NULL){
  if(gp=="dirichlet"){
    I.vec <- as.numeric(xi.vec > delta)
  } else if(gp=="logit.norm"){
    p.vec <- 1/(1 + exp(-xi.vec))
    # Make sure all documents have at least one topic
    I.vec <- 0
    while(sum(I.vec)<1){I.vec <- sapply(p.vec,rbinom,n=1,size=1)}
  } else if(gp=="mv.probit"){
    I.vec <- as.numeric(xi.vec > 0)
    # Make sure all documents have at least one topic
    if(sum(I.vec)<1){I.vec[which.max(xi.vec)] <- 1}
  }
    
  return(I.vec)
}

gen.theta.vec <- function(xi.vec,I.vec,gp="dirichlet"){
  # Renormalize active topics
  if(gp=="dirichlet"){
    # Zero out inactive topics
    xi.vec[I.vec==0] <- 0
    theta.vec <- xi.vec/sum(xi.vec)
  } else if(any(gp=="logit.norm",gp=="mv.probit")){
    exp.xi.vec <- exp(xi.vec-mean(xi.vec))
    # Zero out inactive topics
    exp.xi.vec[I.vec==0] <- 0
    theta.vec <- exp.xi.vec/sum(exp.xi.vec)
  }
  return(theta.vec)
}

# Cute function to count how many topics will have
count.active.topics <- function(nlevels,J){
  sum <- 0
  for(i in 1:nlevels){sum <- sum + J^i}
  return(sum)
}

##################################################3

# Functions to create trees

## child.gen <- function(nlevels,rate){
##   nchild.old <-  rpois(1,rate)
##   names(nchild.old) <- paste("1.",c(1:length(nchild.old)),sep="")
##   for(i in 1:(nlevels-1)){
##     nchild.new <- rpois(nchild.old,rate)
##   }
## }

# Generate document lengths

# Function to generate document lengths
# Old version is pretty weird---built simpler version below
## doc.lgen <- function(D,nlevels,psi,V,L,mult=2000,size=10){
##   mean.length <- exp(psi)*mult*V
##   out <- rnbinom(n=D,size=size,mu=mean.length)/L
##   return(out)
## }

# New doc length generation function. Variance of lengths is
# a decreasing function of 'shape'
doc.lgen <- function(D,shape,L=1){
  doc.norm.lengths <- L*rgamma(D,shape,shape)
  return(doc.norm.lengths)
}


# Generate hyperparameters

# Function to draw discrimination parameters for entire hierarchy for
# one word
#tau2.dist <- function(n,nu,sigma2){rinvchisq(n=n,df=nu,scale=sigma2)}

tau2.dist <- function(n,nu,sigma2,dist="inv.chisq"){
  if (dist=="inv.chisq") {draws <- rinvchisq(n=n,df=nu,scale=sigma2)
  } else if (dist=="log.normal") {draws <-
                                  exp(rnorm(n=n,mean=nu,sd=sqrt(sigma2)))}
}

tau2.list.draw <- function(nchild,nu,sigma2){
  tau2.L0.list <- tau2.dist(n=1,nu=nu,sigma2=sigma2)
  tau2.L1.list <- tau2.dist(n=nchild,nu=nu,sigma2=sigma2)
  tau2.L2.list <- replicate(n=nchild,
                            tau2.dist(n=nchild,nu=nu,sigma2=sigma2),
                            simplify=FALSE)
  
  names(tau2.L0.list) <- "0"
  names(tau2.L1.list) <- 1:nchild
  for(i in 1:nchild){
    names(tau2.L2.list[[i]]) <- paste(i,".",1:nchild,sep="")
  }
  
  tau2.list <- list(L0=tau2.L0.list,L1=tau2.L1.list,L2=tau2.L2.list)
  return(tau2.list)
}


# Function to draw discrimination parameters for entire hierarchy for
# all words
tau2.param.gen <- function(V,nchild,nu,sigma2){
  tau2.param <- replicate(n=V,tau2.list.draw(nchild=nchild,nu=nu,
                            sigma2=sigma2),simplify=FALSE)
  return(tau2.param)
}


mu.vec.draw <- function(nchild,mean.vec,var.vec){
  mu.vec <- mapply(rnorm,mean=mean.vec,sd=sqrt(var.vec),
                   MoreArgs=list(n=nchild),SIMPLIFY=FALSE)
  return(mu.vec)
}

mu.list.draw <- function(nchild,psi,gamma,tau2.param.word){
  tau2.L0 <- tau2.param.word[[1]]
  tau2.L1.list <- tau2.param.word[[2]]
  tau2.L2.list <- tau2.param.word[[3]]
  mu.L0 <- rnorm(n=1,mean=psi,sd=gamma)
  mu.L1.list <- rnorm(n=nchild,mean=mu.L0,sd=sqrt(tau2.L0))
  mu.L2.list <- mu.vec.draw(nchild=nchild,mean.vec=mu.L1.list,
                            var.vec=tau2.L1.list)
  mu.L3.list <- mapply(mu.vec.draw,mean.vec=mu.L2.list,
                       var.vec=tau2.L2.list,
                       MoreArgs=list(nchild=nchild),
                       SIMPLIFY=FALSE)
  names(mu.L0) <- 0
  names(mu.L1.list) <- 1:nchild
  for(i in 1:nchild){
    names(mu.L2.list[[i]]) <- paste(i,".",1:nchild,sep="")
  }
  for(i in 1:nchild){
    for(j in 1:nchild){
      names(mu.L3.list[[i]][[j]]) <- paste(i,".",j,".",1:nchild,sep="")
    }
    names(mu.L3.list[[i]]) <- NULL
  }
  mu.list <- list(L0=mu.L0,L1=mu.L1.list,L2=mu.L2.list,
                  L3=mu.L3.list)
  return(mu.list)
}

# Function to draw discrimination parameters for entire hierarchy for
# all words
mu.param.gen <- function(nchild,psi,gamma,tau2.param.list){
  mu.param <- lapply(tau2.param.list,mu.list.draw,nchild=nchild,
                     psi=psi,gamma=gamma)
  return(mu.param)
}

# Function to generate matrix of rate parameters and vector
# of corpus level parameters
get.mu.matrix <- function(mu.param.list){
  mu.param.vecs <- t(sapply(mu.param.list,function(list){
    comp.vec <- unlist(list)
    # get rid of corpus rate
    vec <- comp.vec[-1]
    return(vec)}))
  mu.corpus.vec <- sapply(mu.param.list,function(list){
    comp.vec <- unlist(list)
    # select corpus rate
    mu.0 <- comp.vec[1]
    return(mu.0)})
  names(mu.corpus.vec) <- NULL
  return(list(mu.param.vecs=mu.param.vecs,mu.corpus.vec=mu.corpus.vec))
}

get.tau2.matrix <- function(tau2.param.list){
  tau2.param.vecs <- t(sapply(tau2.param.list,function(list){
    vec <- unlist(list)
    return(vec)}))
  return(tau2.param.vecs)
}


# Create data address book
gen.topic.address.book <- function(topics){
  topic.address.book <- c()
  for (topic in topics){
    topic.parse <- unlist(strsplit(topic,"\\."))
    level <- as.numeric(unlist(strsplit(topic.parse[1],""))[2])
    if (level==1) {parent <- "CORPUS"}
    else {
      p1 <- paste("L",level-1,sep="")
      p2 <- paste(topic.parse[-c(1,length(topic.parse))],
                              collapse=".")
      parent <- paste(p1,p2,sep=".")
    }
    topic.address.book <- rbind(topic.address.book,c(topic,level,parent))
  }
  colnames(topic.address.book) <- c("topic","level","parent")
  return(topic.address.book)
}

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


# Function to ready count data for output
sparse.counts <- function(count.vec){
  pos.counts <- which(count.vec>0)
  nonzero.counts <- count.vec[pos.counts]
  word.strings <- mapply(function(label,count){paste(label,count,sep=":")},
                       label=pos.counts,count=nonzero.counts)
  out.string <- paste(word.strings,collapse=" ")
  return(out.string)
}

# Function to write LDA style output
write.lda.data <- function(file.out,topics,I.vecs,count.word.doc.mat,
                           doc.lengths,doc.ids=NULL){
  
  # Get labels for each document
  D <- nrow(I.vecs)
  if(is.null(doc.ids)){doc.ids <- 1:D}
  else{if(length(doc.ids)!=D){stop("doc.ids must be same length as number of requested documents")}}
  doc.topic.labels <- apply(I.vecs,1,function(vec){topics[as.logical(vec)]})
  doc.topic.label.strings <- sapply(doc.topic.labels,paste,collapse=" ")

  # Get sparse representation for word counts
  count.strings <- apply(count.word.doc.mat,1,sparse.counts)

  # Get output lines for file
  output.lines <- mapply(function(id,topic.labels,length,count.string){
    outstr <- paste(id,topic.labels,length,count.string,sep="\t")
    outstr <- paste(outstr,"\n",sep="")
    return(outstr)},
                         id=doc.ids,topic.labels=doc.topic.label.strings,
                         length=doc.lengths,count.string=count.strings)

  #print(output.lines[1])

  # Write output lines to file
  # Make sure file doesn't already exist
  if(file.exists(file.out)){
    system(paste("rm",file.out))
  }
  lapply(output.lines,cat,file=file.out,append=TRUE)
}

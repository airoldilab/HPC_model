# Script to plot the joint posterior of the tau2 and phi parameters

args <- commandArgs(TRUE)
analysis.dir <- args[1]
file.ave.params <- args[2]
file.kept.word.stems <- args[3]

restrict.quant <- FALSE

#load("../mmm_fits/fit_train500/ave_params_gibbs.RData")
#file.kept.word.stems <- "kept_word_id_stems500.txt"

tp.dir <- file.path(analysis.dir,"tau2_phi_plots/")

# Load in logit function
logit <- function(x){log(x)-log(1-x)}

# Get kept word ids
load(file.ave.params)
word.stem.table <- read.table(file.kept.word.stems,header=FALSE,
                              row.names=1,sep="\t")
kept.word.ids <- rownames(word.stem.table)

# Extract mu, tau2, and phi parameters for kept words
mu.param.vecs <- ave.param.list$mu.param.vecs[kept.word.ids,]
mu.word.ids <- rownames(mu.param.vecs)
logit.phi.param.vecs <- logit(ave.param.list$phi.param.vecs[kept.word.ids,])
logit.phi.parent.param.vecs <- logit(ave.param.list$phi.parent.param.vecs[kept.word.ids,])
phi.word.ids <- rownames(logit.phi.param.vecs)
log.tau2.param.vecs <- log(ave.param.list$tau2.param.vecs[kept.word.ids,])
tau2.word.ids <- rownames(log.tau2.param.vecs)

# Get list of topics
parent.topics <- colnames(log.tau2.param.vecs)
topics <-  colnames(mu.param.vecs)
parent.child.list <- ave.param.list$parent.child.list
parent.one.child <- sapply(parent.child.list,length) == 1
parent.topics <- parent.topics[!parent.one.child]

# Load up topic dictionary
topic.dict <- read.table("~/reuters_prj/mmm_analysis_code/topic_codes.txt",
                         sep="\t",row.names=1,as.is=TRUE)

# Plot discrim power and frequency for topic
for (topic in parent.topics){
  topic.name <- topic.dict[topic,]
  child.topics <-parent.child.list[[topic]]
  nchild <- length(child.topics)
  tau2.vec <- rep(log.tau2.param.vecs[,topic],nchild)
  logit.phi.vec <- as.vector(logit.phi.param.vecs[,child.topics])

  if(restrict.quant){
  # Get quantiles of each dimension
    quant.tau2 <- quantile(tau2.vec,probs=c(0.01,0.95,0.99))
    quant.phi <- quantile(logit.phi.vec,probs=c(0.001,0.95,0.99))

    phi.keep.full <- logit.phi.vec > quant.phi[1]
  #tau2.keep.full <- tau2.vec > quant.tau2[1]
    tau2.keep.full <- phi.keep.full
    index.keep.full <- apply(cbind(tau2.keep.full,phi.keep.full),1,all)
    
    tau2.vec.full <- tau2.vec[index.keep.full]
    logit.phi.vec.full <- logit.phi.vec[index.keep.full]
  } else {
    tau2.vec.full <- tau2.vec
    logit.phi.vec.full <- logit.phi.vec
  }
  

  # Set up tp plot
  title.plot <- paste("Differential-Exclusivity plot for",topic.name)
  title.pdf <- paste(tp.dir,"tp_plot_",topic,sep="")
  png(title.pdf,width=8,height=8,units="in",res=300)
  plot(tau2.vec.full,logit.phi.vec.full,main=title.plot,
       ylab=expression(paste("Exclusivity: ",logit(phi[fk]))),
       xlab=expression(paste("Differential usage: ",log(tau2[fk]))),
       cex=0.5)
  dev.off()
  
}

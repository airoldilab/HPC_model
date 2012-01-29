# Script to load in expected rates and find top words for every topic

args <- commandArgs(TRUE)
analysis.dir <- args[1]
file.ave.params <- args[2]
file.kept.word.stems <- args[3]

#load("../mmm_fits/fit_train500/ave_params_gibbs.RData")
#file.kept.word.stems <- "kept_word_id_stems500.txt"

fe.dir <- file.path(analysis.dir,"fe_plots/")
fe.full.dir <- file.path(fe.dir,"full_plots/")
fe.zoom.dir <- file.path(fe.dir,"zoom_plots/")

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
topics <-  colnames(mu.param.vecs)

# Load up topic dictionary
topic.dict <- read.table("~/reuters_prj/mmm_analysis_code/topic_codes.txt",
                         sep="\t",row.names=1,as.is=TRUE)

# Figure out which topic don't have siblings
one.child <- logit.phi.param.vecs[1,] == Inf

#plot(mu.param.vecs[,topic],logit.phi.param.vecs[,topic],main=title.plot,xlab="Frequency",ylab="Exclusivity",ylim=c(-10,5),xlim=c(-20,5))
#identify(mu.param.vecs[,topic],logit.phi.param.vecs[,topic],labels=word.stem.table[,1])

# Plot discrim power and frequency for topic
for (topic in topics){
  topic.name <- topic.dict[topic,]
  mu.vec <- mu.param.vecs[,topic]
  # Use comparison to parent if only one child; use comparison to sibilings if
  # more than one
  if(one.child[topic]){ logit.phi.vec <- logit.phi.parent.param.vecs[,topic]
  } else { logit.phi.vec <- logit.phi.param.vecs[,topic]}

  # Get quantiles of each dimension
  quant.mu <- quantile(mu.vec,probs=c(0.01,0.95,0.99))
  quant.phi <- quantile(logit.phi.vec,probs=c(0.01,0.95,0.99))

  mu.keep.full <- mu.vec > quant.mu[1]
  phi.keep.full <- logit.phi.vec > quant.phi[1]
  index.keep.full <- apply(cbind(mu.keep.full,phi.keep.full),1,all)
  
  mu.vec.full <- mu.vec[index.keep.full]
  logit.phi.vec.full <- logit.phi.vec[index.keep.full]

  # Set up full plot
  title.plot <- paste("Frequency-Exclusivity plot for",topic.name)
  title.pdf <- paste(fe.full.dir,"fe_plot_",topic,sep="")
  png(title.pdf,width=8,height=8,units="in",res=300)
  plot(mu.vec.full,logit.phi.vec.full,main=title.plot,
       ylab=expression(paste("Exclusivity: ",logit(phi[fk]))),
       xlab=expression(paste("Frequency: ",mu[fk])),cex=0.5)
  dev.off()

  # Get zoom plots - all points in top 5% of both dimensions
  mu.keep.zoom <- mu.vec > quant.mu[2]
  phi.keep.zoom <- logit.phi.vec > quant.phi[2]
  index.keep.zoom <- apply(cbind(mu.keep.zoom,phi.keep.zoom),1,all)
  
  mu.vec.zoom <- mu.vec[index.keep.zoom]
  logit.phi.vec.zoom <- logit.phi.vec[index.keep.zoom]

  # Figure out which points to label - top 5% of either dimension
  mu.label.zoom <- mu.vec > quant.mu[2]
  phi.label.zoom <- logit.phi.vec > quant.phi[2]
  index.label.zoom1 <- apply(cbind(mu.label.zoom,phi.label.zoom),1,any)
  index.label.zoom <- apply(cbind(index.label.zoom1,index.keep.zoom),1,all)

  mu.vec.label <- mu.vec[index.label.zoom]
  logit.phi.vec.label <- logit.phi.vec[index.label.zoom]
  labels.zoom <- word.stem.table[index.label.zoom,1]

  cex.plot <- 0.6
  title.plot <- paste("Upper 1% of Frequency-Exclusivity plot for",topic.name)
  title.pdf <- paste(fe.zoom.dir,"fe_zoom_plot_",topic,sep="")
  png(title.pdf,width=8,height=8,units="in",res=300)
  plot(mu.vec.zoom,logit.phi.vec.zoom,main=title.plot,
       ylab=expression(paste("Exclusivity: ",logit(phi[fk]))),
       xlab=expression(paste("Frequency: ",mu[fk])),cex=cex.plot)
  text(mu.vec.label, logit.phi.vec.label, labels = labels.zoom, adj = NULL,
       pos = 4, offset = 0.25, vfont = NULL,
       cex = cex.plot, col = NULL, font = NULL)
  dev.off()

  
}

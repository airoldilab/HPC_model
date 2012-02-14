# Script to plot distribution of tau2 parameters by level

args <- commandArgs(TRUE)
analysis.dir <- args[1]
file.ave.params <- args[2]
file.kept.word.stems <- args[3]

tau2.dist.dir <- file.path(analysis.dir,"tau2_dist_plots/")

# Get kept word ids
load(file.ave.params)
word.stem.table <- read.table(file.kept.word.stems,header=FALSE,
                              row.names=1,sep="\t")
kept.word.ids <- rownames(word.stem.table)

# Extract mu, tau2, and phi parameters for kept words
log.tau2.param.vecs <- log(ave.param.list$tau2.param.vecs[kept.word.ids,])
tau2.word.ids <- rownames(log.tau2.param.vecs)


## # Load in list of kept stop words
## kept.stop.word.file <- paste(analysis.dir,"kept_stop_words",cutoff,".txt",
##                              sep="")
## kept.stop.word.ids.raw <- as.character(read.table(kept.stop.word.file)[,1])

## kept.stop.word.ids <- kept.stop.word.ids.raw[kept.stop.word.ids.raw
##                                          %in% rownames(log.tau2.param.vecs)]
## stop.word.pos <- sapply(kept.stop.word.ids,function(id){which(rownames(log.tau2.param.vecs)==id)})

## # Get sets of stop and reg word tau2s
## stop.word.tau2 <- log.tau2.param.vecs[stop.word.pos,]
## reg.word.tau2 <- log.tau2.param.vecs[-stop.word.pos,]


# Get list of topics
parent.topics <- colnames(log.tau2.param.vecs)
topics <-  colnames(log.tau2.param.vecs)
parent.child.list <- ave.param.list$parent.child.list
parent.one.child <- sapply(parent.child.list,length) == 1
parent.topics <- parent.topics[!parent.one.child]

# Load up topic dictionary
topic.dict <- read.table("~/reuters_prj/mmm_analysis_code/topic_codes.txt",
                         sep="\t",row.names=1,as.is=TRUE)

# Plot discrim power and frequency for topic
for (topic in parent.topics){
  if(topic=="CORPUS"){topic.name <- topic
  } else {topic.name <- topic.dict[topic,]}
  child.topics.cand <- parent.child.list[[topic]]
  child.topics <- child.topics.cand[child.topics.cand %in% parent.topics]
  nchild <- length(child.topics)
  
  if(nchild > 0){
    ## print(topic)
    ## print(child.topics)
    ## print("end\n")
    tau2.parent.vec <- rep(log.tau2.param.vecs[,topic],nchild)
    tau2.child.vec <- as.vector(log.tau2.param.vecs[,child.topics])
    cor.tau <- round(cor(tau2.parent.vec,tau2.child.vec),3)
    
    # Set up tp plot
    title.plot <- paste("Differential use in ",topic.name,
                        " vs. its child topics",sep="")
                        #"(cor=",cor.tau,")"
    title.png <- paste(tau2.dist.dir,"tau2_dist_plot_",topic,".png",sep="")
    png(title.png,width=7,height=7,units="in",res=200)
    plot(tau2.child.vec~tau2.parent.vec,main=title.plot,
         ylab=expression(paste("Child differential usage: ",log(tau[fk]^{2}))),
         xlab=expression(paste("Parent differential usage: ",log(tau[fp]^{2}))),
         cex=0.5,col=rgb(red=0,green=0,blue=0,alpha=0.3))
    curve.loess <- predict(loess(tau2.child.vec~tau2.parent.vec))
    order.x <- order(tau2.parent.vec)
    mat.loess <- cbind(tau2.parent.vec[order.x],curve.loess[order.x])
    lines(mat.loess, lwd=2, col="red")
    dev.off()
  }
  
}

# Script to plot distribution of tau2 parameters by topics and stop
# word status

library("vioplot")

args <- commandArgs(TRUE)
analysis.dir <- args[1]
file.ave.params <- args[2]
cutoff <- args[3]
file.kept.word.stems <- args[4]

weight.phi <- 0.5
stop.reg.dir <- paste(analysis.dir,"stop_reg_plots/",sep="")

## cutoff <- 500
## main.dir <- "/n/airoldifs1/jbischof/reuters_output/"
## fit.dir <- paste(main.dir,"mmm_fits/fit_train",cutoff,"/",sep="")
## analysis.dir <- paste(main.dir,"mmm_analysis_out/",sep="")

# Load in logit function
logit <- function(x){log(x)-log(1-x)}
load(file.ave.params)

word.stem.table <- read.table(file.kept.word.stems,header=FALSE,
                              row.names=1)
kept.word.ids <- rownames(word.stem.table)

# Load in model fit
load(file.ave.params)
## tau2.param.vecs <- ave.param.list$tau2.param.vecs
## log.tau2.param.vecs <- log(tau2.param.vecs)
mu.param.vecs <- ave.param.list$mu.param.vecs[kept.word.ids,]
logit.phi.param.vecs <- logit(ave.param.list$phi.param.vecs[kept.word.ids,])
logit.phi.parent.param.vecs <- logit(ave.param.list$phi.parent.param.vecs[kept.word.ids,])

#word.ids <- rownames(mu.param.vecs)

# Load in list of kept stop words
kept.stop.word.file <- paste(analysis.dir,"kept_stop_words",cutoff,".txt",
                             sep="")
kept.stop.word.ids.raw <- as.character(read.table(kept.stop.word.file)[,1])

kept.stop.word.ids <- kept.stop.word.ids.raw[kept.stop.word.ids.raw
                                         %in% rownames(mu.param.vecs)]
stop.word.pos <- sapply(kept.stop.word.ids,function(id){which(rownames(mu.param.vecs)==id)})


# Get list of topics
topics <-  colnames(mu.param.vecs)

# Figure out which topic don't have siblings
one.child <- logit.phi.param.vecs[1,] == Inf

# Load up topic dictionary
topic.dict <- read.table("~/reuters_prj/mmm_analysis_code/topic_codes.txt",
                         sep="\t",row.names=1,as.is=TRUE)

fe.score.vecs <- matrix(NA,length(kept.word.ids),length(topics),
                        dimnames=list(kept.word.ids,topics))
mu.score.vecs <- fe.score.vecs
phi.score.vecs <- fe.score.vecs


# Standardize variables and get requested convex combination
for (topic in topics){
  #topic.name <- topic.dict[topic,]
  mu.vec <- mu.param.vecs[,topic]
  # Use comparison to parent if only one child; use comparison to sibilings if
  # more than one
  if(one.child[topic]){ logit.phi.vec <- logit.phi.parent.param.vecs[,topic]
  } else { logit.phi.vec <- logit.phi.param.vecs[,topic]}

  # Get quantile evaluations for data
  mu.vec.quant <- ecdf(mu.vec)(mu.vec) 
  logit.phi.quant <- ecdf(logit.phi.vec)(logit.phi.vec)

  # Get requested average and store in matrix
  fe.score <- 1/(weight.phi/mu.vec.quant + (1-weight.phi)/logit.phi.quant)
  fe.score.vecs[,topic] <- logit(fe.score)
  mu.score.vecs[,topic] <- logit(mu.vec.quant)
  phi.score.vecs[,topic] <- logit(logit.phi.quant)
}


# Get sets of stop and reg word scores
stop.word.fe <- fe.score.vecs[stop.word.pos,]
reg.word.fe <- fe.score.vecs[-stop.word.pos,]
stop.word.max.fe <- apply(stop.word.fe,1,max)
reg.word.max.fe <- apply(reg.word.fe,1,max)

stop.word.mu <- mu.score.vecs[stop.word.pos,]
reg.word.mu <- mu.score.vecs[-stop.word.pos,]
stop.word.max.mu <- apply(stop.word.mu,1,max)
reg.word.max.mu <- apply(reg.word.mu,1,max)

stop.word.phi <- phi.score.vecs[stop.word.pos,]
reg.word.phi <- phi.score.vecs[-stop.word.pos,]
stop.word.max.phi <- apply(stop.word.phi,1,max)
reg.word.max.phi <- apply(reg.word.phi,1,max)

xlab.phi <- expression(paste("logit ECDF of ",phi[fk]))
xlab.mu <- expression(paste("logit ECDF of ",mu[fk]))
xlab.fe <- expression(paste("logit ECDF of ",FE[fk]))
main.phi <- "Density of maximum exclusivity ECDF over all topics"
main.mu <- "Density of maximum frequency ECDF over all topics"
main.fe <- "Density of maximum FE score over all topics"

# Density plots
filename <- paste(stop.reg.dir,"reg_stop_word_fe_comp.png",sep="")
png(filename,width=7,height=7,units="in",res=200)
plot(density(stop.word.max.fe),col="red",lty=1,xlab=xlab.fe,
     main=main.fe)
lines(density(reg.word.max.fe),lty=1,col="black")
## curve(ecdf(stop.word.fe)(x),6,10,col="red",lty=1,xlab="logit FE score",
##        main="Density of FE score for regular words v. stop words")
## curve(ecdf(reg.word.fe)(x),6,10,col="black",lty=1,add=TRUE)
legend(x="topleft",legend=c("regular words","stop words"),lty=1,
       col=c("black","red"))
dev.off()

filename <- paste(stop.reg.dir,"reg_stop_word_mu_comp.png",sep="")
png(filename,width=7,height=7,units="in",res=200)
plot(density(reg.word.max.mu),col="black",lty=1,xlab=xlab.mu,
     main=main.mu)
lines(density(stop.word.max.mu),lty=1,col="red")
legend(x="topright",legend=c("regular words","stop words"),lty=1,
       col=c("black","red"))
dev.off()

filename <- paste(stop.reg.dir,"reg_stop_word_phi_comp.png",sep="")
png(filename,width=7,height=7,units="in",res=200)
plot(density(stop.word.max.phi),col="red",lty=1,
     xlab=xlab.phi,main=main.phi)
lines(density(reg.word.max.phi),lty=1,col="black")
legend(x="topright",legend=c("regular words","stop words"),lty=1,
       col=c("black","red"))
dev.off()

filename <- paste(stop.reg.dir,"reg_stop_word_comp.png",sep="")
png(filename,width=14,height=7,units="in",res=200)
par(mfrow=c(1,2))
plot(density(reg.word.max.mu),col="black",lty=1,
     xlab=xlab.mu,xlim=c(0,10.5),main=main.mu)
lines(density(stop.word.max.mu),lty=1,col="red")
legend(x="topright",legend=c("regular words","stop words"),lty=1,
       col=c("black","red"))
plot(density(stop.word.max.phi),col="red",lty=1,xlab=xlab.phi,#xlim=c(0,11),
     main=main.phi)
lines(density(reg.word.max.phi),lty=1,col="black")
legend(x="topright",legend=c("regular words","stop words"),lty=1,
       col=c("black","red"))
dev.off()



filename <- paste(stop.reg.dir,"reg_stop_word_fe_hist.png",sep="")
png(filename,width=7,height=7,units="in",res=200)
hist(stop.word.max.fe,
     border="red",lty=1,xlab=xlab.fe,xlim=c(0,10),
     main=main.fe,freq=FALSE)
hist(reg.word.max.fe,lty=1,xlim=c(0,10),
     border="black",add=TRUE,freq=FALSE)
legend(x="topleft",legend=c("regular words","stop words"),lty=1,
       col=c("black","red"))
dev.off()

filename <- paste(stop.reg.dir,"reg_stop_word_mu_hist.png",sep="")
png(filename,width=7,height=7,units="in",res=200)
hist(reg.word.max.mu,
     border="black",
     lty=1,xlab=xlab.mu,xlim=c(0,11),
     main=main.mu,freq=FALSE)
hist(stop.word.max.mu,lty=1,xlim=c(0,11),
     border="red",
     add=TRUE,freq=FALSE)
legend(x="topright",legend=c("regular words","stop words"),lty=1,
       col=c("black","red"))
dev.off()

filename <- paste(stop.reg.dir,"reg_stop_word_phi_hist.png",sep="")
png(filename,width=7,height=7,units="in",res=200)
hist(stop.word.max.phi,
     border="red",
     lty=1,xlab=xlab.phi,#xlim=c(0,10),
     main=main.phi,freq=FALSE)
hist(reg.word.max.phi,lty=1,#xlim=c(0,10),
     border="black",
     add=TRUE,freq=FALSE)
legend(x="topright",legend=c("regular words","stop words"),lty=1,
       col=c("black","red"))
dev.off()


filename <- paste(stop.reg.dir,"reg_stop_word_hist.png",sep="")
png(filename,width=14,height=7,units="in",res=200)
par(mfrow=c(1,2))
hist(reg.word.max.mu,
     border="black",
     lty=1,xlab=xlab.mu,xlim=c(0,11),
     main=main.mu,freq=FALSE)
hist(stop.word.max.mu,lty=1,xlim=c(0,11),
     border="red",
     add=TRUE,freq=FALSE)
legend(x="topright",legend=c("regular words","stop words"),lty=1,
       col=c("black","red"))
hist(stop.word.max.phi,
     border="red",
     lty=1,xlab=xlab.phi,#xlim=c(0,10),
     main=main.phi,freq=FALSE)
hist(reg.word.max.phi,lty=1,#xlim=c(2,10),
     border="black",
     add=TRUE,freq=FALSE)
legend(x="topright",legend=c("regular words","stop words"),lty=1,
       col=c("black","red"))
dev.off()


## # Overall violinplot
## filename <- paste(stop.reg.dir,"by_stop_word_vp.png",sep="")
## png(filename,width=7,height=7,units="in",res=200)
## vioplot(reg.word.max.tau2,stop.word.max.tau2,
##         names=c("Regular Words","Stop Words"))
## title("Density of tau2 for regular words v. stop words")
## title(ylab="tau2")
## dev.off()

## filename <- paste(stop.reg.dir,"by_stop_word_log_vp.png",sep="")
## png(filename,width=7,height=7,units="in",res=200)
## vioplot(log(reg.word.max.tau2),log(stop.word.max.tau2),
##         names=c("Regular Words","Stop Words"))
## title("Density of tau2 for regular words v. stop words")
## title(ylab="log tau2")
## dev.off()

## probs <- seq(0,1,0.05)
## out.mat <- rbind(quantile(reg.word.max.tau2,probs=probs),quantile(stop.word.max.tau2,probs=probs))
## rownames(out.mat) <- c("reg words","stop words")
## print(t(out.mat))

#par(mfrow=c(2,1))
#hist(stop.word.max.tau2,freq=FALSE,breaks='fd',xlim=c(0,20))
#hist(reg.word.max.tau2,freq=FALSE,breaks='fd')

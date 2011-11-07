# Script to plot distribution of tau2 parameters by topics and stop
# word status

library("vioplot")

args <- commandArgs(TRUE)
analysis.dir <- args[1]
file.ave.params <- args[2]
cutoff <- args[3]


## cutoff <- 250
## main.dir <- "/n/airoldifs1/jbischof/reuters_output/"
## fit.dir <- paste(main.dir,"mmm_fits/fit_train",cutoff,"/",sep="")
## analysis.dir <- paste(main.dir,"mmm_analysis_out/",sep="")


# Load in model fit
load(file.ave.params)
tau2.param.vecs <- ave.param.list$tau2.param.vecs

# Load in list of kept stop words
kept.stop.word.file <- paste(analysis.dir,"kept_stop_words",cutoff,".txt",
                             sep="")
kept.stop.word.ids.raw <- as.character(read.table(kept.stop.word.file)[,1])

kept.stop.word.ids <- kept.stop.word.ids.raw[kept.stop.word.ids.raw
                                         %in% rownames(tau2.param.vecs)]
stop.word.pos <- sapply(kept.stop.word.ids,function(id){which(rownames(tau2.param.vecs)==id)})

# Get sets of stop and reg word tau2s
stop.word.tau2 <- tau2.param.vecs[stop.word.pos,]
reg.word.tau2 <- tau2.param.vecs[-stop.word.pos,]

stop.word.max.tau2 <- apply(stop.word.tau2,1,max)
reg.word.max.tau2 <- apply(reg.word.tau2,1,max)


filename <- paste(analysis.dir,"by_stop_word_tau2_density_log.png",sep="")
png(filename,width=7,height=7,units="in",res=300)
plot(density(log(reg.word.max.tau2)),col="red",xlab="log tau2",
     main="Density of log tau2 for regular words v. stop words")
lines(density(log(stop.word.max.tau2)),lty=2,col="blue")
legend(x="topright",legend=c("regular words","stop words"),lty=c(1,2),
       col=c("red","blue"))
dev.off()

filename <- paste(analysis.dir,"by_stop_word_tau2_density.png",sep="")
png(filename,width=7,height=7,units="in",res=300)
plot(density(reg.word.max.tau2),col="red",xlab="tau2",
     main="Density of tau2 for regular words v. stop words",
     ylim=c(0,max(density(stop.word.max.tau2)$y)))
lines(density(stop.word.max.tau2),lty=2,col="blue")
legend(x="topright",legend=c("regular words","stop words"),lty=c(1,2),
       col=c("red","blue"))
dev.off()

# Overall violinplot
filename <- paste(analysis.dir,"by_stop_word_vp.png",sep="")
png(filename,width=7,height=7,units="in",res=300)
vioplot(reg.word.max.tau2,stop.word.max.tau2,
        names=c("Regular Words","Stop Words"))
title("Density of tau2 for regular words v. stop words")
title(ylab="tau2")
dev.off()

probs <- seq(0,1,0.05)
out.mat <- rbind(quantile(reg.word.max.tau2,probs=probs),quantile(stop.word.max.tau2,probs=probs))
rownames(out.mat) <- c("reg words","stop words")
print(t(out.mat))

#par(mfrow=c(2,1))
#hist(stop.word.max.tau2,freq=FALSE,breaks='fd',xlim=c(0,20))
#hist(reg.word.max.tau2,freq=FALSE,breaks='fd')

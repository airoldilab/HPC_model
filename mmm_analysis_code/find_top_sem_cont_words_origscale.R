# Script to load in expected phis and find top words for every topic

# PROBLEM WITH THIS SCRIPT THAT COMPONENTS IN HARMONIC MEAN MUST BE POSITIVE

args <- commandArgs(TRUE)
fit.dir <- args[1]
analysis.dir <- args[2]
file.ave.params <- args[3]
file.kept.word.stems <- args[4]
n.get <- args[5]
weight.phi <- as.numeric(args[6])

# Load in sorting function
funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_analysis_code/mmm_analysis_functions/"
source(paste(funct.dir,"get_top_items.R",sep=""))
fit.funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_fit_code/mmm_fit_functions/"


load(file.ave.params)

word.stem.table <- read.table(file.kept.word.stems,header=FALSE,
                              row.names=1)
kept.word.ids <- rownames(word.stem.table)

mu.param.vecs <- exp(ave.param.list$mu.param.vecs[kept.word.ids,])
mu.word.ids <- rownames(mu.param.vecs)
phi.param.vecs <- ave.param.list$phi.param.vecs[kept.word.ids,]
phi.parent.param.vecs <- ave.param.list$phi.parent.param.vecs[kept.word.ids,]
phi.word.ids <- rownames(phi.param.vecs)

# Get list of topics
topics <-  colnames(mu.param.vecs)

# Figure out which topic don't have siblings
one.child <- phi.param.vecs[1,] == 1

# Load up topic dictionary
topic.dict <- read.table("~/reuters_prj/mmm_analysis_code/topic_codes.txt",
                         sep="\t",row.names=1,as.is=TRUE)

sem.cont.vecs <- matrix(NA,length(kept.word.ids),length(topics),
                        dimnames=list(kept.word.ids,topics))


# Standardize variables and get requested convex combination
for (topic in topics){
  #topic.name <- topic.dict[topic,]
  mu.vec <- mu.param.vecs[,topic]
  # Use comparison to parent if only one child; use comparison to sibilings if
  # more than one
  if(one.child[topic]){ phi.vec <- phi.parent.param.vecs[,topic]
  } else { phi.vec <- phi.param.vecs[,topic]}

  # Standardize mu and phi vectors
  mu.vec <- (mu.vec-mean(mu.vec))/sd(mu.vec)
  phi.vec <- (phi.vec-mean(phi.vec))/sd(phi.vec)

  # Get requested convex combination and store in matrix
  sem.cont.score <- 1/(weight.phi/phi.vec + (1-weight.phi)/mu.vec)
  sem.cont.vecs[,topic] <- sem.cont.score
}



top.sem.cont <- t(apply(sem.cont.vecs,2,get.top.items,word.id.vec=phi.word.ids,
                   word.stem.table=word.stem.table,n.get=n.get))
#top.sem.cont <- cbind(top.sem.cont[,1],topic.dict[,1],top.sem.cont[,-1])
file.out <- paste(analysis.dir,"top_sem_cont.txt",sep="")
write.table(top.sem.cont,file.out,quote=FALSE,sep=" ",col.names=FALSE)

# Script to load in expected phis and find top words for every topic

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

# Load in logit function
logit <- function(x){log(x)-log(1-x)}

load(file.ave.params)

word.stem.table <- read.table(file.kept.word.stems,header=FALSE,
                              row.names=1)
kept.word.ids <- rownames(word.stem.table)

mu.param.vecs <- ave.param.list$mu.param.vecs[kept.word.ids,]
mu.word.ids <- rownames(mu.param.vecs)
logit.phi.param.vecs <- logit(ave.param.list$phi.param.vecs[kept.word.ids,])
logit.phi.parent.param.vecs <- logit(ave.param.list$phi.parent.param.vecs[kept.word.ids,])
phi.word.ids <- rownames(logit.phi.param.vecs)

# Get list of topics
topics <-  colnames(mu.param.vecs)

# Figure out which topic don't have siblings
one.child <- logit.phi.param.vecs[1,] == Inf

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
  if(one.child[topic]){ logit.phi.vec <- logit.phi.parent.param.vecs[,topic]
  } else { logit.phi.vec <- logit.phi.param.vecs[,topic]}

  # Standardize mu and phi vectors
  mu.vec <- (mu.vec-mean(mu.vec))/sd(mu.vec)
  logit.phi.vec <- (logit.phi.vec-mean(logit.phi.vec))/sd(logit.phi.vec)

  # Get requested convex combination and store in matrix
  sem.cont.score <- 1/(weight.phi/exp(logit.phi.vec) + (1-weight.phi)/exp(mu.vec))
  sem.cont.vecs[,topic] <- sem.cont.score
}



top.sem.cont <- t(apply(sem.cont.vecs,2,get.top.items,word.id.vec=phi.word.ids,
                   word.stem.table=word.stem.table,n.get=n.get))
top.sem.cont <- cbind(topic.dict[topics,1],top.sem.cont)
file.out <- paste(analysis.dir,"top_sem_cont.txt",sep="")
write.table(top.sem.cont,file.out,quote=FALSE,sep=" ",col.names=FALSE)

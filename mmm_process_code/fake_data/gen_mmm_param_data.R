# Script to process generation of mmm parameters

# Load generation functions
source.dir <- "/n/home13/jbischof/reuters_prj/mmm_process_code/mmm_process_functions/"
#source(paste(source.dir,"gen_mmm_data.R",sep=""))
source(paste(source.dir,"gen_mmm_data_functions.R",sep=""))
source(paste(source.dir,"gen_mmm_params.R",sep=""))

args <- commandArgs(TRUE)
file.tab <- args[1]
file.true.params <- args[2]

# Size of parameter space
ndocs <- 10000
nwords <- 500
# Number of levels after corpus level
nlevels <- 3
# Number of children per parent
nchildren <- 4

# Membership distribution
K <- count.active.topics(nlevels,nchildren)
eta.vec <- rnorm(n=K)-4
lambda2 <- 2
## Create as much negative correlation as theoretically possible
## Sigma <- lambda2*diag(K) - (lambda2/(K+1))*matrix(1,K,K)
## print(eigen(Sigma)$values)

# Prior on corpus rate
psi <- log(0.5)
gamma <- 1
# Prior for the discrim parameters -- not the same as my prior, but here we want
# a descriptive process rather than to enforce shrinkage
nu <- 4
sigma2 <- 0.05

outlist.mmm.params <- gen.mmm.params(nchildren=nchildren,nlevels=nlevels,
                                     ndocs=ndocs,nwords=nwords,
                                     psi=psi,gamma=gamma,
                                     eta.vec=eta.vec,lambda2=lambda2,
                                     nu=nu,sigma2=sigma2,
                                     gp="logit.norm",
                                     verbose=TRUE)

true.param.list <- outlist.mmm.params$true.param.list
topic.address.book <- outlist.mmm.params$topic.address.book

# Write topic address book file
#file.topic.address.book <- "mmm_topic_address_book.txt"
write.table(topic.address.book,file=file.tab,
            row.names=FALSE,quote=FALSE,
            sep="\t")

# Save true parameters to check fitting routine
#file.param.out <- "mmm_true_params.RData"
save(true.param.list,file=file.true.params)



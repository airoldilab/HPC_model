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
# Use unrestricted covariance matrix?
full.Sigma <- FALSE
K <- count.active.topics(nlevels,nchildren)
eta.vec <- rnorm(n=K)-3
lambda2 <- 2
if(full.Sigma){
  # Start with correlation matrix
  cor.mat <- diag(K)
  # Create one block of topics positively correlated with each other and
  # negative correlated with everyone else
  # Size of block
  block.size <- 10
  # Maximum negative correlation
  max.corr <- 1/(K+1)
  cor.mat[1:block.size,1:block.size] <- matrix(0.5,ncol=block.size,nrow=block.size) + diag(rep(0.5,block.size))
  # Put negative correlation on off diagonal
  cor.mat[(block.size + 1):K,1:block.size] <- -max.corr
  cor.mat[1:block.size,(block.size + 1):K] <- -max.corr
  # Scale Sigma up to be covariance matrix
  Sigma <- lambda2*cor.mat
  # Check that Sigma actually positive definite
  print("Eigenvalues of Sigma matrix:")
  print(eigen(Sigma)$values)
  
} else {
  Sigma <- NULL
}
## Create as much negative correlation as theoretically possible
## Sigma <- lambda2*diag(K) - (lambda2/(K+1))*matrix(1,K,K)
## print(eigen(Sigma)$values)

# Prior on corpus rate
psi <- log(0.5)
gamma <- 1
# Prior for the discrim parameters
nu <- 4
sigma2 <- 0.05

outlist.mmm.params <- gen.mmm.params(nchildren=nchildren,nlevels=nlevels,
                                     ndocs=ndocs,nwords=nwords,
                                     psi=psi,gamma=gamma,
                                     eta.vec=eta.vec,lambda2=lambda2,
                                     full.Sigma=full.Sigma,Sigma=Sigma,
                                     nu=nu,sigma2=sigma2,
                                     gp="logit.norm",verbose=TRUE)

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



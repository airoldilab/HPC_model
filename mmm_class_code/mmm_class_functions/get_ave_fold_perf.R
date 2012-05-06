# Load in classification performances across fold for a model and get
# the average and standard deviation

args <- commandArgs(TRUE)
nfolds <- as.numeric(args[1])
model <- args[2]


folds <- 1:nfolds
if(model=="mmm"){
  folds <- folds[-7]
  nfolds <- nfolds - 1
}

for( fold in folds ) {
  main.dir <- paste("/n/airoldifs1/jbischof/reuters_output/mmm_folds/fold",
                    fold,"/",sep="")
  topic.class.dir <- paste(main.dir,"mmm_class_out/topic_class_out/",sep="")
  fold.perf.file <- paste(topic.class.dir,"reuters_",model,
                     "_class_onethres.txt",sep="")

  fold.perf.table <- as.matrix(read.table(fold.perf.file,
                                          row.names=1,header=TRUE))

  if(fold == 1){
    # Set up array to hold results
    perf.array <- array(NA,dim=c(dim(fold.perf.table),nfolds),
                    dimnames=append(dimnames(fold.perf.table),list(folds)))
  }

  perf.array[,,as.character(fold)] <- fold.perf.table
}

# Get mean and SD of fold perf
fold.perf.ave <- round(apply(perf.array,1:2,mean),4)
fold.perf.sd <- round(apply(perf.array,1:2,sd),4)

# Write output to disk
out.dir <- "/n/airoldifs1/jbischof/reuters_output/mmm_folds/"
out.ave.file <- paste(out.dir,model,"_fold_perf_ave.txt",sep="")
write.table(fold.perf.ave,file=out.ave.file,quote=FALSE,sep="\t")
out.sd.file <- paste(out.dir,model,"_fold_perf_sd.txt",sep="")
write.table(fold.perf.sd,file=out.sd.file,quote=FALSE,sep="\t")


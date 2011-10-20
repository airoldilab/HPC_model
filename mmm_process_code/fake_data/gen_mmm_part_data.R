# Script to process generation of mmm data validation and test sets
# Note that giving every fold the same membership parameters here

# Load generation functions
source.dir <- "/n/home13/jbischof/reuters_prj/mmm_process_code/mmm_process_functions/"
source(paste(source.dir,"gen_mmm_data.R",sep=""))
source(paste(source.dir,"gen_mmm_data_functions.R",sep=""))

args <- commandArgs(TRUE)
true.param.file <- args[1]
out.dir <- args[2]

# Load true parameters from original data set
#load("mmm_true_params.RData")
load(true.param.file)

ndocs <- nrow(true.param.list$theta.param.vecs)
part.list <- c("train","valid","test")

# Generate data from model
for ( i in 1:length(part.list) ){
  part <- part.list[i]
  doc.ids <- c(1:ndocs) + ndocs*(i-1)
  lda.filename <- paste(out.dir,"mmm_data_ldaformat_",part,".txt",sep="")
  kept.words <- gen.mmm.data(true.param.list=true.param.list,ndocs=ndocs,
                             lda.filename=lda.filename,shape=5,
                             doc.ids=doc.ids)

  # Get list of kept word ids
  if ( part == "train" ){
    file.kept.words <- paste(out.dir,"kept_word_ids.txt",sep="")
    write.table(kept.words,col.names=FALSE,row.names=FALSE,
                file=file.kept.words)
  }

}



## # Validation data
## lda.filename.valid <- paste(out.file.prefix,"valid",".txt",sep="")
## lda.filename.valid <- "mmm_data_ldaformat_valid.txt"
## gen.mmm.data(true.param.list=true.param.list,ndocs=ndocs.valid,
##              lda.filename=lda.filename.valid,
##              doc.ids=2001:4000,shape=5)

## # Test data
## lda.filename.valid <- paste(out.file.prefix,"test",".txt",sep="")
## lda.filename.test <- "mmm_data_ldaformat_test.txt"
## gen.mmm.data(true.param.list=true.param.list,ndocs=ndocs.test,
##              lda.filename=lda.filename.test,
##              doc.ids=4001:6000,shape=5)

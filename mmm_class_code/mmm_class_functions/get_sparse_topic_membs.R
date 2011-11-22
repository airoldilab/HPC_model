# Function to get lda gamma files in sparse representation

args <- commandArgs(TRUE)
main.dir <- args[1]

#main.dir <- "/n/airoldifs1/jbischof/reuters_output/"
raw.data.dir <- paste(main.dir,"mmm_raw_data/",sep="")
out.file <- paste(raw.data.dir,"reuters_topic_membs_sparse.RData",sep="")

library("Matrix")

# Go through partitions, load up data, and save in sparse format

for (partition in c("train","valid","test")) {

  # Read in partition data
  file.part.data <- paste(raw.data.dir,"reuters_",partition,"_topic_membs.txt",sep="")
  part.data <- as.matrix(read.table(file.part.data,row.names=1,header=TRUE))

  # Create sparse version of data
  assign(paste(partition,".topic.membs",sep=""),as(part.data,"sparseMatrix"))

  # Remove all large objects from memory and gc
  rm(part.data)
  gc()

}

# Save partition data to file
save(train.topic.membs,valid.topic.membs,test.topic.membs,file=out.file)

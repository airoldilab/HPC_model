# Load in logit membership probabilities for each topic and cat into one table

args <- commandArgs(TRUE)
main.dir <- args[1]
cutoff <- args[2]

# Find all the files associated with the classification
#main.dir <- "/n/airoldifs2/lab/jbischof/reuters_output/"
class.dir <- paste(main.dir,"mmm_class_out/",sep="")
raw.data.dir <- paste(main.dir,"mmm_raw_data/",sep="")
out.dir <- paste(class.dir,"topic_class_out/",sep="")
liblinear.dir <- paste(class.dir,"logit_liblinear_fits",cutoff,"/",sep="")

# Load in topic address book and figure out which topic to use
topic.address.book <- read.table("reuters_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)
topics <- topic.address.book[,"topic"]


for( partition in c("valid","test") ) {

  # Output file name
  file.out.root <- paste("reuters_logit_liblinear_",partition,"_prob.txt",sep="")
  file.out <- paste(out.dir,file.out.root,sep="")

  # Load in document ids for test partition
  file.part.docids <- paste(raw.data.dir,"reuters_",partition,"_docids.txt",sep="")
  part.doc.ids <- scan(file=file.part.docids,what=integer(0))
  
  topic.prob.vecs <- c()
  for( topic in topics ) {
    file.load.root <- paste(partition,"_",topic,".out",sep="")
    file.load <- paste(liblinear.dir,file.load.root,sep="")
    print(file.load)
    topic.prob.table <- read.table(file.load,as.is=TRUE,header=TRUE)
    if(ncol(topic.prob.table)==2){
      topic.prob.vecs <- cbind(topic.prob.vecs,0)
    } else {
      topic.probs <- topic.prob.table[,3]
      topic.prob.vecs <- cbind(topic.prob.vecs,topic.probs)
    }
  }
  
  #topic.prob.vecs <- t(topic.prob.vecs)
  rownames(topic.prob.vecs) <- part.doc.ids
  colnames(topic.prob.vecs) <- topics

  # Output overall table
  write.table(topic.prob.vecs,file=file.out)


}

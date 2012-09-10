# Load in SVM membership predictions for each topic and cat into one table

args <- commandArgs(TRUE)
main.dir <- args[1]
cutoff <- args[2]

# Find all the files associated with the classification
#main.dir <- "/n/airoldifs2/lab/jbischof/reuters_output/"
class.dir <- paste(main.dir,"mmm_class_out/",sep="")
raw.data.dir <- paste(main.dir,"mmm_raw_data/",sep="")
out.dir <- paste(class.dir,"topic_class_out/",sep="")
liblinear.dir <- paste(class.dir,"svm_liblinear_fits",cutoff,"/",sep="")
#out.dir <- liblinear.dir

# Output file name
file.out.root <- paste("reuters_svm_liblinear_test_pred.txt",sep="")
file.out <- paste(out.dir,file.out.root,sep="")

# Load in document ids for test partition
file.part.docids <- paste(raw.data.dir,"reuters_test_docids.txt",sep="")
test.doc.ids <- scan(file=file.part.docids,what=integer(0))

# Load in topic address book and figure out which topic to use
topic.address.book <- read.table("reuters_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)
topics <- topic.address.book[,"topic"]

topic.pred.vecs <- c()
for( topic in topics ) {
  file.load.root <- paste("test_",topic,".out",sep="")
  file.load <- paste(liblinear.dir,file.load.root,sep="")
  print(file.load)
  topic.preds <- t(read.table(file.load,as.is=TRUE))
  topic.pred.vecs <- rbind(topic.pred.vecs,topic.preds)
}


## # Figure out which files to load
## file.root <- paste("test_",sep="")
## files.out.dir <- dir(liblinear.dir)
## pos.files.to.load <- grep(file.root,files.out.dir)
## files.to.load <- files.out.dir[pos.files.to.load]

## topic.pred.vecs <- c()

## for(file in files.to.load){
##   print(file)
##   filename <- paste(liblinear.dir,file,sep="")
##   topic.preds <- t(read.table(filename,as.is=TRUE))
##   topic.pred.vecs <- rbind(topic.pred.vecs,topic.preds)
## }

topic.pred.vecs <- t(topic.pred.vecs)
rownames(topic.pred.vecs) <- test.doc.ids
colnames(topic.pred.vecs) <- topics

# Output overall table
write.table(topic.pred.vecs,file=file.out)




# Script to get classification diagnostics for MMM using only one threshold

# Get command line arguments
args <- commandArgs(TRUE)
print(args)
model <- args[1]
main.dir <- args[2]
cutoff <- args[3]

print(paste("Model name is:",model))

#main.dir <- "/n/airoldifs2/lab/jbischof/reuters_output/"
raw.data.dir <- paste(main.dir,"mmm_raw_data/",sep="")
class.dir <- paste(main.dir,"mmm_class_out/",sep="")
out.dir <- paste(class.dir,"topic_class_out/",sep="")
#roc.dir <- paste(class.dir,"roc/",sep="")
library("ROCR")
library("Matrix")

# Set weight for precision in F1 measure
alpha <- 0.5

# Load in topic address book and figure out which topic to use
topic.address.book <- read.table("reuters_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)
topics <- topic.address.book[,"topic"]

# Load topic membs file---data has names '[partition].topic.membs'
topic.membs.file <- paste(raw.data.dir,"reuters_topic_membs_sparse.RData",sep="")
load(topic.membs.file)

# Get standard order for documents
docids.valid <- rownames(valid.topic.membs)
docids.test <- rownames(test.topic.membs)

# Load in fitted membership probs
# Need to take care of mmm separately
if(model=="mmm"){
  # Valid data
  file.valid.data <- paste(class.dir,"valid_class_",cutoff,"/final_class.txt",sep="")
  valid.topic.probs <- read.table(file.valid.data,header=FALSE,row.names=1)[docids.valid,]
  colnames(valid.topic.probs) <- topics
  
  # Test data
  file.test.data <- paste(class.dir,"test_class_",cutoff,"/final_class.txt",sep="")
  test.topic.probs <- read.table(file.test.data,header=FALSE,row.names=1)[docids.test,]
  colnames(test.topic.probs) <- topics
  

} else if(model=="lab_lda") {
  # Valid data
  file.class.valid.params <- paste(out.dir,"reuters_",model,"_valid_prob.txt",sep="")
  valid.topic.probs <- as.matrix(read.table(file.class.valid.params,row.names=1,header=TRUE)[docids.valid,])
  # Test data
  file.class.test.params <- paste(out.dir,"reuters_",model,"_test_prob.txt",sep="")
  test.topic.probs <- as.matrix(read.table(file.class.test.params,row.names=1,header=TRUE)[docids.test,])
  
} else if(model=="svm") {
  file.class.test.preds <- paste(out.dir,"reuters_svm_liblinear_test_pred.txt",sep="")
  test.thres.memb.mat <- read.table(file.class.test.preds,row.names=1,header=TRUE)
  test.thres.memb.mat <- as.matrix(test.thres.memb.mat[docids.test,])
  test.thres.memb <- as.vector(test.thres.memb.mat)

} else if(model=="logit") {
  # Valid data
  file.class.valid.params <- paste(out.dir,"reuters_logit_liblinear_valid_prob.txt",sep="")
  valid.topic.probs <- read.table(file.class.valid.params,row.names=1,header=TRUE)
  valid.topic.probs <- valid.topic.probs[docids.valid,]
  # Test data
  file.class.test.params <- paste(out.dir,"reuters_logit_liblinear_test_prob.txt",sep="")
  test.topic.probs <- read.table(file.class.test.params,row.names=1,header=TRUE)
  test.topic.probs <- test.topic.probs[docids.test,]
}

if(model!="svm"){
  # Need to get vector of topic loadings and vector of actual memberships in same order
  # Get standard order for columns and make sure only use topics for which have fit
  topics.labeled <- colnames(valid.topic.membs)
  topics.fit <- colnames(valid.topic.probs)
  topics.use <- topics.labeled[topics.labeled %in% topics.fit]
} else {topics.use <- topics}

# Get vector of true labels for topics---note that get vector by columns
y.valid.mat <- valid.topic.membs[docids.valid,topics.use]
y.test.mat <- test.topic.membs[docids.test,topics.use]
y.valid.vec <- as.vector(y.valid.mat)
y.test.vec <- as.vector(y.test.mat)

# Check for empty topics and add a random (single) member to
# smooth things over for now
tot.membs <- colSums(y.test.mat)
zero.membs <- which(tot.membs==0)
if(length(zero.membs)>0){
  for ( pos in zero.membs ){
    y.test.mat[1,pos] <- 1
  }
}

# Want labels for test vectors by topic so can break down results
test.vec.topic.labels <- rep(topics.use,each=length(docids.test))
n.doc.test <- length(docids.test)
n.topics <- length(topics.use)
test.topic.pos <- list()
for (i in 1:n.topics){
  topic <- topics.use[i]
  test.topic.pos[[topic]] <- c(1:n.doc.test) + (i - 1)*n.doc.test
}

if (model != "svm") {
  # Get vector predicted topic memb probs from model
  fitted.valid.probs.vec <- as.vector(valid.topic.probs[docids.valid,topics.use])
  fitted.test.probs.vec <- as.vector(test.topic.probs[docids.test,topics.use])

  # Get prediction objects from ROCR
  pred.topic.valid <- prediction(fitted.valid.probs.vec,y.valid.vec)
  pred.topic.test <- prediction(fitted.test.probs.vec,y.test.vec)

  # Get f1 measure for validation set from ROCR 
  perf.topic.valid.f1 <- performance(pred.topic.valid,"f",alpha=alpha)

  # Get plot of performance by threshold
  pdf(paste(out.dir,"f1_thres_plot.pdf",sep=""))
  plot(perf.topic.valid.f1)
  dev.off()

  # Get pref object for test set
  test.pred <- prediction(fitted.test.probs.vec,y.test.vec)
  test.perf <- performance(test.pred,"tpr","fpr")
  test.perf.pr <- performance(test.pred,"prec","rec")

  # Get AUC measure for test set
  test.perf.auc <- performance(test.pred,"auc")
  test.auc <- test.perf.auc@y.values[[1]]

  # Get best threshold from validation set
  pos.thres.best <- which.max(perf.topic.valid.f1@y.values[[1]])
  thres.best <- perf.topic.valid.f1@x.values[[1]][pos.thres.best]
  # Use threshold to get predictions for test set and get perf obj
  test.thres.memb <- as.numeric(fitted.test.probs.vec > thres.best)

} else {test.auc <- 0}


test.pred.out <- prediction(test.thres.memb,y.test.vec)
test.perf.out.f1 <- performance(test.pred.out,"f",alpha=alpha)
test.perf.out <- performance(test.pred.out,"prec","rec")
test.f1 <- test.perf.out.f1@y.values[[1]][2]
test.prec <- test.perf.out@y.values[[1]][2]
test.rec <- test.perf.out@x.values[[1]][2]
test.true.pos <- test.pred.out@tp[[1]][2]
test.n.pos <- test.pred.out@n.pos[[1]]
test.n.pos.pred <- test.pred.out@n.pos.pred[[1]][2]

test.perf.mat <- matrix(c(test.prec,test.rec,test.f1,test.auc,
test.n.pos,test.n.pos.pred,test.true.pos),nrow=1)

colnames(test.perf.mat) <- c("precision","recall","f1","auc","n_pos",
                                   "n_pos_pred","true_pos")
rownames(test.perf.mat) <- "micro"


# Now extract performance measures by topic
test.topic.perf.mat <- c()
for (topic in topics.use){
  pos.topic <- test.topic.pos[[topic]]
  test.thres.memb.topic <- test.thres.memb[pos.topic]
  y.test.topic <- y.test.vec[pos.topic]
  test.pred.out <- prediction(test.thres.memb.topic,y.test.topic)
  test.perf.out.f1 <- performance(test.pred.out,"f",alpha=alpha)
  test.perf.out <- performance(test.pred.out,"prec","rec")
  test.f1 <- test.perf.out.f1@y.values[[1]][2]
  test.prec <- test.perf.out@y.values[[1]][2]
  test.rec <- test.perf.out@x.values[[1]][2]
  test.true.pos <- test.pred.out@tp[[1]][2]
  test.n.pos <- test.pred.out@n.pos[[1]]
  test.n.pos.pred <- test.pred.out@n.pos.pred[[1]][2]
  test.perf.topic <- matrix(c(test.prec,test.rec,test.f1,test.auc,
                                  test.n.pos,test.n.pos.pred,test.true.pos),nrow=1)
  rownames(test.perf.topic) <- topic
  test.topic.perf.mat <- rbind(test.topic.perf.mat,test.perf.topic)
}

# Get macro-averaged performance
macro.perf <- apply(test.topic.perf.mat,2,mean)

# Get single matrix of results
test.perf.mat <- rbind(test.perf.mat,"macro"=macro.perf,test.topic.perf.mat)


# Write output to file
out.file.root <- paste("reuters_",model,"_class_onethres",sep="")
out.file <- paste(out.dir,out.file.root,".txt",sep="")
write.table(test.perf.mat,file=out.file,quote=FALSE,sep="\t")

print(test.perf.mat)


# Write model performance object to RData file
## file.model.perf.root <- paste("reuters_",model,"_perf_onethres",sep="")
## file.model.perf <- paste(out.dir,file.model.perf.root,".RData",sep="")
## save(test.perf,test.perf.pr,file=file.model.perf)

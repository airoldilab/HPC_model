# Script to process observed data into form that fitting function can use
# Need to load a lot less if just classification

process.observed.data <- function(filename.doc.topic,
                                  filename.doc.word.count,
                                  filename.doc.length,
                                  filename.feature.word.count=NULL,
                                  filename.none.docs=NULL,
                                  L=140,classify=FALSE){

  if(!classify){if(is.null(filename.feature.word.count)){
    stop("Need to provide feature.word.count filename if not doing classification.")}
                load(filename.feature.word.count)}
  load(filename.doc.topic)
  load(filename.doc.word.count)
  doc.length.table <- read.table(filename.doc.length,as.is=TRUE)
  doc.length.vec <- doc.length.table[,2]
  names(doc.length.vec) <- doc.length.table[,1]
  doc.length.vec <- doc.length.vec/L

  # Make sure doc.length.vec and doc.count.list have same order
  doc.length.vec <- doc.length.vec[names(doc.count.list)]

  # May have to clean up documents
  clean <- !is.null(filename.none.docs)
  if(clean){
    # Fix up doc.count.list and feature.count.list---
    # don't want any of the 'none' docs in there
    none.docs.vec <- read.table(filename.none.docs,as.is=TRUE)[,1]
    doc.count.ids <- names(doc.count.list)
    pos.none.docs <- sapply(none.docs.vec,function(doc.id){
      which(doc.count.ids==doc.id)})
    doc.count.list <- doc.count.list[-pos.none.docs]
    doc.length.vec <- doc.length.vec[names(doc.count.list)]
    feature.count.list <- lapply(feature.count.list,
                                 function(wc.vec){
                                      docs <- names(wc.vec)
                                      match.na <- match(docs,none.docs.vec)
                                      if(!all(is.na(match.na))){
                                        wc.vec <- wc.vec[is.na(match.na)]}
                                      return(wc.vec)})
  
    # Clean up doc.topic.list
    doc.topic.list <- lapply(doc.topic.list,function(topic.vec){
      pos.c1511 <- which(topic.vec=="C1511")
      if(length(pos.c1511 > 0)){topic.vec <- topic.vec[-pos.c1511]}
      return(topic.vec)})
  }

  data.list <- list(doc.count.list=doc.count.list,
                    doc.topic.list=doc.topic.list,
                    doc.length.vec=doc.length.vec)

  if(!classify){data.list$feature.count.list <- feature.count.list}

  return(data.list)
  
}

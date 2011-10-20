# Script to read in LDA format data and create list format

read.lda.data <- function(filename,outfile.other,outfile.feature,
                          outfile.doc,freq.cutoff=1,L=1,verbose=FALSE){
  #filename <- paste(dir,filename,sep="")
  command <- paste("wc -l", filename)
  com.out <- system(command,intern=TRUE)
  nlines <- as.numeric(unlist(strsplit(com.out," "))[1])
  con <- file(description=filename,open="r")

  # Want counts by feature and by doc
  doc.count.list <- list()
  feature.count.list <- list()
  doc.length.vec <- c()
  doc.id.vec <- c()
  doc.topic.list <- list()
  for(i in 1:nlines){
    if(verbose){print(paste("Processing document",i))}
    # Load in next line
    line.vec <- scan(file=con,what="character",nlines=1,sep="\t",quiet=TRUE)
    doc.id <- line.vec[1]
    topic.str <- line.vec[2]
    doc.length <- line.vec[3]

    # Read in and parse string of counts
    count.vec <- c()
    id.vec <- c()
    topic.str <- line.vec[2]
    topic.vec <- unlist(strsplit(topic.str," "))
    doc.topic.list[[doc.id]] <- topic.vec
    doc.id.vec <- c(doc.id.vec,doc.id)
    doc.length.vec <- c(doc.length.vec,as.numeric(line.vec[3]))
    count.str <- line.vec[4]
    count.str.vec <- unlist(strsplit(count.str," "))
    for(item in count.str.vec){
      id.count.vec <- unlist(strsplit(item,":"))
      id <- id.count.vec[1]
      count <- as.numeric(id.count.vec[2])
      id.vec <- c(id.vec,id)
      count.vec <- c(count.vec,count)
      
      # Add count to feature specific list
      # Create new entry if feature not seen before
      if(is.null(feature.count.list[[id]])){
        feature.count.list[[id]] <- count
        names(feature.count.list[[id]]) <- doc.id
      }
      # Else update existing entry
      else{
        countf.vec <- feature.count.list[[id]]
        countf.vec <- c(countf.vec,count)
        names(countf.vec)[length(countf.vec)] <- doc.id
        feature.count.list[[id]] <- countf.vec
      }
    }
    names(count.vec) <- id.vec
    doc.count.list[[doc.id]] <- count.vec
  }
  
  #close(con)

  # Do initial feature selection using given frequency cutoff
  if(freq.cutoff != 1){
    word.counts <- sapply(feature.count.list,sum)
    word.ids.pos.keep <- which(word.counts > freq.cutoff)
    word.ids.keep <- names(word.counts)[word.ids.pos.keep]
  }

  # Now remove discarded words from doc and feature count lists
  feature.count.list <- feature.count.list[word.ids.keep]
  doc.count.list <- lapply(doc.count.list,function(count.list){
    word.ids <- names(count.list)
    pos.keep <- which(word.ids %in% word.ids.keep)
    out.count.list <- count.list[pos.keep]
    return(out.count.list)
  })

  # Normalize doc.length.vec
  if(L != 1){doc.length.vec <- doc.length.vec/L}
  
  # Create output files for different data types
  names(doc.length.vec) <- doc.id.vec
  save(doc.length.vec,doc.topic.list,file=outfile.other)
  save(feature.count.list,file=outfile.feature)
  save(doc.count.list,file=outfile.doc)
}



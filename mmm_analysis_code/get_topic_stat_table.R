topic_tab <- read.table("topic_membs.txt",sep="\t",header=TRUE)
xtab.topics <- xtable(topic_tab)
print(xtab.topics,include.rownames=FALSE)


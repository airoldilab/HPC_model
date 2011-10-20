# Script to do the "data processing" for the Rmpi example
# Trying to mimic the basics of my problem

data <- matrix(rnorm(1000000),1000,1000)
save(data,file="data.RData")

# Master script to get draws from predictive distribution of labels
# for withheld documents

# Extract the command line arguments for number of nodes and relevant folders
args <- commandArgs(TRUE)
print(args)
slave.data.dir <- args[1]
slave.id <- args[2]
cutoff <- args[3]
out.dir <- args[4]

## cutoff <- 500
## partition <- "valid"
## main.dir <- "/n/airoldifs1/jbischof/reuters_output/"
## slave.data.dir <- paste(main.dir,"mmm_class_out/",partition,
##                  "_slave_data",cutoff,"/",sep="")
## slave.id <- 1


# Load in fitting functions
sprintft <- function(x,...){return(paste(date(),": ",sprintf(x,...),sep=""))}
funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_fit_code/mmm_fit_functions/"
source(paste(funct.dir,"initialize_params.R",sep=""))
source(paste(funct.dir,"xi_update.R",sep=""))
source(paste(funct.dir,"xi_mcmc.R",sep=""))
source(paste(funct.dir,"case_control_samp.R",sep=""))
source("/n/home13/jbischof/reuters_prj/hmc/hmc_functions.R")
process.funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_process_code/mmm_process_functions/"
source(paste(process.funct.dir,"gen_mmm_data_functions.R",sep=""))
class.funct.dir <- "/n/home13/jbischof/reuters_prj/mmm_class_code/mmm_class_functions/"
source(paste(class.funct.dir,"label_import_sampler.R",sep=""))

# Load parameter list
file.current.param.list <- paste(slave.data.dir,"current_param_list.RData",sep="")
load(file.current.param.list)

# Load slave data
file.slave.data <- paste(slave.data.dir,"slave_data",slave.id,".RData",sep="")
load(file.slave.data)

# Load job list
file.joblist <- paste(slave.data.dir,"class_joblist.RData",sep="")
load(file.joblist)

# Load in data address book
topic.address.book <- read.table("reuters_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)

# Get list of job.ids
job.ids <- xi.job.list[[slave.id]]

# Write output lines to file
file.out <- paste(out.dir,"class_data",slave.id,".txt",sep="")
# Make sure file doesn't already exist; if so, don't repeat jobs
if(file.exists(file.out)){
  old.file <- read.table(file.out,header=FALSE,as.is=TRUE,row.names=1,sep="\t")
  done.docs <- rownames(old.file)
  for( doc.id in done.docs ){
    if(doc.id %in% job.ids){
      id.pos <- which(job.ids == doc.id)
      job.ids <- job.ids[-id.pos]
    }
  }
  ## system(paste("rm",file.out))
}

## # Write header for output file
## topics <- colnames(current.param.list$xi.param.vecs)
## midstr <- paste(c("",topics),collapse="\t")
## outstr <- paste(midstr,"\n",sep="")
## cat(outstr,file=file.out,append=TRUE)



# Get importance samples of label vector or xi vector
n.docs <- length(job.ids)
for( job.id in job.ids ) {

  # Time update
  t0 <-  proc.time()[3]
        
  import.out <- label.import.sampler(job.id=job.id,current.param.list=current.param.list,
                                     doc.length.vec=doc.length.vec,
                                     doc.count.list=doc.count.list,
                                     ndraws.import.samp=300,
                                     return.expect=TRUE,
                                     hmc.burnin=100,
                                     hmc.step.size=0.04,
                                     hmc.nsteps=50,
                                     hmc.debug=FALSE)
  
  midstr <- paste(import.out$xi.expect,collapse="\t")
  outstr <- paste(job.id,"\t",midstr,"\n",sep="")
  cat(outstr,file=file.out,append=TRUE)

  # Record time diff
  t1 <- proc.time()[3]

  cat(sprintft("Finished doc %s in %f seconds\n",job.id,t1-t0))
}






## attach(import.out)
## plot(xi.draws[,10],type="b")
## xi.opt[order(xi.opt)]
## col.means <- colMeans(xi.draws)
## plot(col.means[order(col.means)])
## col.means[order(col.means)]

## plot(xi.draws[,"C15"],type="b")
## plot(xi.draws[,"MCAT"],type="b")

## hist(xi.draws[,"M11"],'fd')

## doc.topic.list[[job.id]]

## I.expect[order(I.expect)]
## xi.expect[order(xi.expect)]

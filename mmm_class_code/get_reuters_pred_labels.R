# Master script to get draws from predictive distribution of labels
# for withheld documents

# Extract the command line arguments for number of nodes and relevant folders
args <- commandArgs(TRUE)
out.dir <- args[1]
slave.id <- args[2]
cutoff <- args[3]

cutoff <- 500
partition <- "valid"
main.dir <- "/n/airoldifs1/jbischof/reuters_output/"
out.dir <- paste(main.dir,"mmm_class_out/",partition,
                 "_slave_data",cutoff,"/",sep="")
slave.id <- 1


# Load in fitting functions
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
file.current.param.list <- paste(out.dir,"current_param_list.RData",sep="")
load(file.current.param.list)

# Load slave data
file.slave.data <- paste(out.dir,"slave_data",slave.id,".RData",sep="")
load(file.slave.data)

# Load job list
file.joblist <- paste(out.dir,"fit_joblist.RData",sep="")
load(file.joblist)

# Load in data address book
topic.address.book <- read.table("reuters_topic_address_book.txt",
                                 header=TRUE,as.is=TRUE)

job.ids <- xi.job.list[[slave.id]]
job.id <- job.ids[1]
current.param.list$full.Sigma <- FALSE

# Get importance samples of label vector
debug(label.import.sampler)
import.out <- label.import.sampler(job.id=job.id,current.param.list=current.param.list,
                                   doc.length.vec=doc.length.vec,
                                   doc.count.list=doc.count.list,
                                   ndraws.import.samp=200,
                                   return.expect=FALSE,
                                   hmc.step.size=0.025,
                                   hmc.nsteps=50,
                                   hmc.debug=FALSE)

plot(xi.draws[,10],type="b")
plot(colMeans(xi.draws))
col.means <- colMeans(xi.draws)
plot(col.means[order(col.means)])
col.means[order(col.means)]

plot(xi.draws[,"GSPO"],type="b")
plot(xi.draws[,"C151"],type="b")


doc.topic.list[[job.id]]

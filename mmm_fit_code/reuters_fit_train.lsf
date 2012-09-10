#!/bin/bash

# Desired queue
#BSUB -q long_parallel

# EDIT THE EMAIL-ADDRESS BELOW TO YOUR FAS EMAIL:
#BSUB -u jbischof@fas.harvard.edu

# THE JOB ARRAY:
#BSUB -J "reuters_fit_train"

# Number of cores requested
#BSUB -n 40

# -R "span[ptile=6]"

#BSUB -a openmpi 

# THE COMMAND TO GIVE TO R, CHANGE TO THE APPROPRIATE FILENAME:
cutoff=250
nodes=40
mpirun -np ${nodes} Rscript reuters_fit.R /n/airoldifs2/lab/jbischof/reuters_output/mmm_fits/fit_train${cutoff}/



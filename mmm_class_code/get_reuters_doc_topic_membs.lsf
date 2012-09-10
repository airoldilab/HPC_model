#!/bin/bash

# DO NOT CHANGE THE QUEUE! YOU **MUST** ONLY USE THE QUEUE: short_serial
#BSUB -q normal_serial

# EDIT THE EMAIL-ADDRESS BELOW TO YOUR FAS EMAIL:
#BSUB -u jbischof@fas.harvard.edu

# THE JOB ARRAY:
#BSUB -J "get_reuters_doc_topic_membs"

main_dir=/n/airoldifs2/lab/jbischof/reuters_output/

# THE COMMAND TO GIVE TO R, CHANGE TO THE APPROPRIATE FILENAME:
python mmm_class_functions/process_get_reuters_doc_topic_membs.py $main_dir

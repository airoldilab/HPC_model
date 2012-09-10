#!/bin/bash

cutoff=500
evan_dir=/n/airoldifs1/egreif/jon/jar-extract/scripts/output/
main_dir=/n/airoldifs2/lab/jbischof/reuters_output/
raw_data_dir=${main_dir}mmm_raw_data/
fit_data_dir=${main_dir}mmm_fits/
class_dir=${main_dir}mmm_class_out/

# Set up a special directory for the folds
main_dir_fold=${main_dir}mmm_folds/


for index in $(seq 1 10)
do
   # Establish directories for each fold
   main_dir_fold_k=${main_dir_fold}fold${index}/
   topic_class_dir_k=${main_dir_fold_k}mmm_class_out/topic_class_out/
   evan_dir_k=${evan_dir}fold${index}/
   
   # Copy files
   cp ${evan_dir_k}reuters_lab_lda_test_prob.txt -t $topic_class_dir_k
   cp ${evan_dir_k}reuters_lab_lda_valid_prob.txt -t $topic_class_dir_k
   
done

exit 0

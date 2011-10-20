#!/bin/bash

cutoff=250
main_dir=/n/airoldifs1/jbischof/reuters_output/
raw_data_dir=${maindir}mmm_raw_data/

# LSF templates
test_valid_template=process_test_valid_lda_data_template.lsf
train_template=process_train_lda_data_template.lsf

# Special directory for the folds
main_dir_fold=${main_dir}mmm_folds/


for index in $(seq 1 10)
do
   # Get directories for this fold
   main_dir_fold_k=${main_dir_fold}fold${index}/
   raw_data_dir_k=${main_dir_fold_k}mmm_raw_data/
   fit_dir_k=${main_dir_fold_k}mmm_fits/
   fit_train_dir_k=${fit_dir_k}fit_train/
   class_dir_k=${main_dir_fold_k}mmm_class_out/
   
   # Create LSF scripts for each partition
   # Validation partition
   echo python ../process_parse_lda_data.py valid $cutoff $main_dir_fold_k | \
      cat ${test_valid_template} - > process_valid_lda_data.lsf
   # Test partition
   echo python ../process_parse_lda_data.py test $cutoff $main_dir_fold_k | \
      cat ${test_valid_template} - > process_test_lda_data.lsf
   # Training partition
   echo python ../process_parse_lda_data.py train $cutoff $main_dir_fold_k | \
      cat ${train_template} - > process_train_lda_data.lsf
   
   # Send off LSF scripts for each partition
   bsub < process_valid_lda_data.lsf
   bsub < process_test_lda_data.lsf
   bsub < process_train_lda_data.lsf
done

exit 0
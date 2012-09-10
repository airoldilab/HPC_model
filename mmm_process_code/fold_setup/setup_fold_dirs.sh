#!/bin/bash

cutoff=500
main_dir=/n/airoldifs2/lab/jbischof/reuters_output/
raw_data_dir=${main_dir}mmm_raw_data/
fit_dir=${main_dir}mmm_fits/
file_lda_clean=${main_dir}reuters_ldaformat_clean.txt
file_init_params=${fit_dir}fit_train${cutoff}/ave_params_gibbs.RData
file_kept_word_ids=${raw_data_dir}reuters_mmm_kept_word_ids${cutoff}.txt

# Set up a special directory for the folds
main_dir_fold=${main_dir}mmm_folds/
mkdir $main_dir_fold


for index in $(seq 1 10)
#for index in 1
do
   main_dir_fold_k=${main_dir_fold}fold${index}/
   raw_data_dir_k=${main_dir_fold_k}mmm_raw_data/
   fit_dir_k=${main_dir_fold_k}mmm_fits/
   fit_train_dir_k=${fit_dir_k}fit_train${cutoff}/
   class_dir_k=${main_dir_fold_k}mmm_class_out/
   mkdir $main_dir_fold_k
   mkdir $raw_data_dir_k
   mkdir $fit_dir_k
   mkdir $fit_train_dir_k
   mkdir $class_dir_k
   
   # Copy over list of kept word ids and initialized parameters
   cp $file_lda_clean -t $main_dir_fold_k
   cp $file_kept_word_ids -t $raw_data_dir_k
   cp $file_init_params $fit_dir_k/ave_param_gibbs_INIT${cutoff}.RData
   cp $file_kept_word_ids -t $raw_data_dir_k
   
   # Create directories for parsed R datasets
   mkdir ${raw_data_dir_k}parsed_valid_data${cutoff}/
   mkdir ${raw_data_dir_k}parsed_train_data${cutoff}/
   mkdir ${raw_data_dir_k}parsed_test_data${cutoff}/
   
   # Run python script to create partition datasets and initialized thetas
   python ../process_data_master_script.py $main_dir_fold_k $cutoff 1
done

exit 0

#!/bin/bash

cutoff=250
main_dir=/n/airoldifs1/jbischof/reuters_output/
raw_data_dir=${main_dir}mmm_raw_data/
file_lda_clean=${main_dir}reuters_ldaformat_clean.txt
file_init_cmu=${raw_data_dir}initialized_corpus_mu${cutoff}.txt
file_init_mu=${raw_data_dir}initialized_mu${cutoff}.txt
file_init_tau2=${raw_data_dir}initialized_tau2${cutoff}.txt
file_kept_word_ids=${raw_data_dir}reuters_mmm_kept_word_ids${cutoff}.txt

# Set up a special directory for the folds
main_dir_fold=${main_dir}mmm_folds/
#mkdir $main_dir_fold


for index in $(seq 1 10)
#for index in $(seq 1 1)
do
   main_dir_fold_k=${main_dir_fold}fold${index}/
   raw_data_dir_k=${main_dir_fold_k}mmm_raw_data/
   fit_dir_k=${main_dir_fold_k}mmm_fits/
   fit_train_dir_k=${fit_dir_k}fit_train/
   class_dir_k=${main_dir_fold_k}mmm_class_out/
   #mkdir $main_dir_fold_k
   #mkdir $raw_data_dir_k
   #mkdir $fit_dir_k
   #mkdir $fit_train_dir_k
   #mkdir $class_dir_k
   
   # Copy over list of kept word ids and initialized parameters
   #cp $file_lda_clean -t $main_dir_fold_k
   cp $file_kept_word_ids -t $raw_data_dir_k
   cp $file_init_cmu -t $raw_data_dir_k
   cp $file_init_mu -t $raw_data_dir_k
   cp $file_init_tau2 -t $raw_data_dir_k
   
   # Create directories for parsed R datasets
   #mkdir ${raw_data_dir_k}parsed_valid_data${cutoff}/
   #mkdir ${raw_data_dir_k}parsed_train_data${cutoff}/
   #mkdir ${raw_data_dir_k}parsed_test_data${cutoff}/
   
   # Run python script to create partition datasets and initialized thetas
   python ../process_data_master_script.py $main_dir_fold_k $raw_data_dir_k ${cutoff}
done

exit 0

#!/bin/bash

cutoff=500
main_dir=/n/airoldifs1/jbischof/reuters_output/
raw_data_dir=${main_dir}mmm_raw_data/
fit_data_dir=${main_dir}mmm_fits/

# Set up a special directory for the folds
main_dir_fold=${main_dir}mmm_folds/

# Set up template
setup_template=reuters_fit_train_setup_template.lsf
fold=TRUE

for index in $(seq 2 10)
#for index in 1
do
   # Establish directories for each fold
   main_dir_fold_k=${main_dir_fold}fold${index}/
   raw_data_dir_k=${main_dir_fold_k}mmm_raw_data/
   fit_dir_k=${main_dir_fold_k}mmm_fits/
   fit_train_dir_k=${fit_dir_k}fit_train${cutoff}/
   class_dir_k=${main_dir_fold_k}mmm_class_out/
   # Create directories for parsed R datasets
   train_data_folder=parsed_train_data${cutoff}/
   
   if [ ! -d $fit_train_dir_k ]
   then
      mkdir $fit_train_dir_k
   fi
   
   # Create LSF script
   echo "R --vanilla --no-save --args 36 30 $fit_train_dir_k $raw_data_dir_k $train_data_folder $cutoff $fold < ../reuters_fit_setup.R" |  \
   cat ${setup_template} - > reuters_fit_train_setup${index}.lsf
   
   # Send off LSF script
   bsub < reuters_fit_train_setup${index}.lsf
   #rm reuters_fit_train_setup${index}.lsf
   
done

exit 0

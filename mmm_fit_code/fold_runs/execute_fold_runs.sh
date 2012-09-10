#!/bin/bash

cutoff=250
main_dir=/n/airoldifs2/lab/jbischof/reuters_output/
raw_data_dir=${main_dir}mmm_raw_data/
fit_data_dir=${main_dir}mmm_fits/

# Set up a special directory for the folds
main_dir_fold=${main_dir}mmm_folds/

# Set up template
fit_template=reuters_fit_train_template.lsf
fold=TRUE

#for index in $(seq 2 10)
for index in 8 10
do
   # Establish directories for each fold
   main_dir_fold_k=${main_dir_fold}fold${index}/
   raw_data_dir_k=${main_dir_fold_k}mmm_raw_data/
   fit_dir_k=${main_dir_fold_k}mmm_fits/
   fit_train_dir_k=${fit_dir_k}fit_train${cutoff}/
   class_dir_k=${main_dir_fold_k}mmm_class_out/
   # Create directories for parsed R datasets
   train_data_folder=parsed_train_data${cutoff}/
   
   # Create LSF script
   # Create LSF script
   echo "mpirun -np 36 Rscript ../reuters_fit.R $fit_train_dir_k $fold" |  \
   cat ${fit_template} - > reuters_fit_train${index}.lsf
   
   # Send off LSF script
   bsub < reuters_fit_train${index}.lsf
   #rm reuters_fit_train${index}.lsf
   
done

exit 0

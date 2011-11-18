#!/bin/bash

cutoff=250
main_dir=/n/airoldifs1/jbischof/reuters_output/
raw_data_dir=${main_dir}mmm_raw_data/
fit_data_dir=${main_dir}mmm_fits/
class_dir=${main_dir}mmm_class_out/


# Special directory for the folds
main_dir_fold=${main_dir}mmm_folds/

# Set up template
run_template=get_liblinear_data_template.lsf

# LSF file to launch data gathering
launch_lsf=get_liblinear_data_fold.lsf

for index in $(seq 1 10)
#for index in 1
do
   # Establish directories for each fold
   main_dir_fold_k=${main_dir_fold}fold${index}/
   raw_data_dir_k=${main_dir_fold_k}mmm_raw_data/
   fit_dir_k=${main_dir_fold_k}mmm_fits/
   fit_train_dir_k=${fit_dir_k}fit_train/
   class_dir_k=${main_dir_fold_k}mmm_class_out/
   ldac_dir_k=${raw_data_dir_k}ldac_data/
   
   # Create directories (if they don't already exist)
   out_dir_k_svm=${class_dir_k}svm_liblinear_fits${cutoff}/
   out_dir_k_logit=${class_dir_k}logit_liblinear_fits${cutoff}/
   data_dir_k=${raw_data_dir_k}liblinear_data/
   
   if [ ! -d $out_dir_k_svm ]
   then
      mkdir $out_dir_k_svm
   fi
   
   if [ ! -d $out_dir_k_logit ]
   then
      mkdir $out_dir_k_logit
   fi
   
   
   if [ ! -d $data_dir_k ]
   then
      mkdir $data_dir_k
   fi
   
#    # Create LSF scripts
#    echo "python ../mmm_class_functions/get_svm_liblinear_data.py $main_dir_fold_k \${LSB_JOBINDEX}" |  \
#    cat ${run_template} - > get_svm_liblinear_data${index}.lsf
   
done

#bsub < ${launch_lsf}

exit 0

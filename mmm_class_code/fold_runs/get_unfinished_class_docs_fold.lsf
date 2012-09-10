#!/bin/bash

# QUEUE
#BSUB -q airoldi

# EDIT THE EMAIL-ADDRESS BELOW TO YOUR FAS EMAIL:
#BSUB -u jbischof@fas.harvard.edu

# THE JOB ARRAY:
#BSUB -J "get_unfinished_class_docs[1-10]"

# Which iteration of the process is this?
iter=1
cat_tag=final_class


partition=test
cutoff=500
main_dir=/n/airoldifs2/lab/jbischof/reuters_output/mmm_folds/fold${LSB_JOBINDEX}/
raw_data_dir=${main_dir}mmm_raw_data/
class_dir=${main_dir}mmm_class_out/
output_dir=${class_dir}${partition}_class_${cutoff}/
filename_lda=${raw_data_dir}/reuters_${partition}_ldaformat.txt
filename_comp=${output_dir}${cat_tag}${iter}.txt
final_filename_comp=${output_dir}${cat_tag}.txt
outfilename=${output_dir}jobs_to_do.txt
outfilename_comp=${output_dir}${cat_tag}_corr.txt

# Create final class file
cat ${output_dir}class_data* > ${filename_comp}
rm ${output_dir}class_data*
cat ${output_dir}${cat_tag}?.txt > ${final_filename_comp}

# Run script
python ../mmm_class_functions/get_unfinished_class_docs.py $filename_lda $final_filename_comp $outfilename $outfilename_comp

# Overwrite final class probabilities with new file where duplicates removed
mv $outfilename_comp $filename_comp
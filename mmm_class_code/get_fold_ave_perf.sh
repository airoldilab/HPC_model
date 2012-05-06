# Script to calculate average fold 

#main_dir=/n/airoldifs1/jbischof/reuters_output/
funct_dir="/n/home13/jbischof/reuters_prj/mmm_class_code/mmm_class_functions/"

#Get arguments to script
model=$1

echo
echo "#########"
echo ${model} fold_ave_perf
Rscript ${funct_dir}get_ave_fold_perf.R 10 $model

exit 0
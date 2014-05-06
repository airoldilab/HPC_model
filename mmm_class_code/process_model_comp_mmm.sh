# Script to coordinate model classification comparison after model-specific
# class probabilities have been generated

#main_dir=/n/airoldifs2/lab/jbischof/reuters_output/
funct_dir="/n/home13/jbischof/reuters_prj/mmm_class_code/mmm_class_functions/"

#Get arguments to script
main_dir=$1
model=$2
cutoff=$3

echo
echo "#########"
echo classify_docs $model
Rscript ${funct_dir}reuters_classify_docs_onethres.R $model $main_dir $cutoff

# echo
# echo "#########"
# echo roc_curves
# Rscript ${funct_dir}get_reuters_roc_curves_onethres.R $main_dir


exit 0
# Script to coordinate model classification comparison after model-specific
# class probabilities have been generated

#main_dir=/n/airoldifs1/jbischof/reuters_output/
funct_dir="/n/home13/jbischof/reuters_prj/mmm_class_code/mmm_class_functions/"

#Get arguments to script
main_dir=$1
cutoff=$2
model=$3

# Output type
if [ "$model" == "svm" ]
then 
   output=pred
else 
   output=probs
fi

echo
echo "#########"
echo ${model}_${output}_process
Rscript ${funct_dir}get_reuters_${model}_${output}_postprocess.R $main_dir $cutoff

echo
echo "#########"
echo classify_docs $model
Rscript ${funct_dir}reuters_classify_docs_onethres.R $model $main_dir

exit 0
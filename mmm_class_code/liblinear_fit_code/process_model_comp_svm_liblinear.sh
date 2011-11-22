# Script to coordinate model classification comparison after model-specific
# class probabilities have been generated

#main_dir=/n/airoldifs1/jbischof/reuters_output/
funct_dir="/n/home13/jbischof/reuters_prj/mmm_class_code/mmm_class_functions/"

#Get arguments to script
main_dir=$1
cutoff=$2

model=svm

# for model in mmm
# do
#    # If this is the mmm model, need to adjust probs by prior odds
#    if [ "$model" == "mmm" ]
#    then
#       echo
#       echo "#########"
#       echo corrected_probs $model
#       Rscript ${funct_dir}get_reuters_mmm_corrected_probs.R $main_dir $cutoff
#    # Otherwise need to postprocess model probs
#    else 
#       echo
#       echo "#########"
#       echo probs_postprocess $model
#       Rscript ${funct_dir}get_reuters_probs_postprocess.R $model $main_dir
#    fi
#    
#    echo
#    echo "#########"
#    echo classify_docs $model
#    Rscript ${funct_dir}reuters_classify_docs_onethres.R $model $main_dir
# done
# 
# # echo
# # echo "#########"
# # echo roc_curves
# # Rscript ${funct_dir}get_reuters_roc_curves_onethres.R $main_dir


echo
echo "#########"
echo svm_pred_process
Rscript ${funct_dir}get_reuters_svm_pred_postprocess.R $main_dir $cutoff

echo
echo "#########"
echo classify_docs $model
Rscript ${funct_dir}reuters_classify_docs_onethres.R $model $main_dir

exit 0
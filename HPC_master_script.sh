## Script to replicate analysis in "Capturing topical content with frequency and exclusivity" by Jonathan Bischof, Edoardo Airoldi at ICML 2012
## Run following LSF scripts
## Most arguments/flags are part of either LSF script or shell script that it calls

## Step 1: extract LDA-type representation of Reuters data for use in fitting scripts
bsub < extract_reuters_data/extract_article.lsf
bsub < extract_reuters_data/get_topic_codes.lsf
bsub < extract_reuters_data/process_get_lda_data.lsf

## Step 2: Split data into training, test, and validation sets
## Process data into Rscript object for fitting functions
bsub < mmm_process_code/process_data_master_script.lsf
bsub < mmm_process_code/process_test_lda_data.lsf
bsub < mmm_process_code/process_train_valid_lda_data.lsf
bsub < mmm_process_code/process_train_lda_data.lsf
bsub < mmm_process_code/process_valid_lda_data.lsf
bsub < mmm_process_code/process_data_master_script2.lsf

## Step 3: Fit model
bsub < mmm_fit_code/reuters_mcmc_setup.lsf
bsub < mmm_fit_code/reuters_mcmc.lsf

## Step 4: Analyze model output
bsub < mmm_analysis_code/process_word_analysis.lsf

## Step 5: Assess classification performance
bsub < mmm_class_code/get_reuters_doc_topic_membs.lsf
bsub < mmm_class_code/get_reuters_pred_labels_setup.lsf
bsub < mmm_class_code/get_reuters_pred_labels.lsf
bsub < mmm_class_code/process_model_comp_mmm.lsf

# Master script to analyze output of model to understand word usage patterns
main_dir=/n/airoldifs1/jbischof/reuters_output/
cutoff=500
fit_dir=${main_dir}mmm_fits/fit_train${cutoff}/
analysis_dir=${main_dir}/mmm_analysis_out/
raw_data_dir=${main_dir}mmm_raw_data/

# Step 1: Extract list of words included in model
# Cut out tokens with less than three characters
python process_word_stems.py $main_dir $cutoff

# Step 2: Get total number of word tokens fit in model
python get_total_word_count.py $main_dir $cutoff

# Step 2: Get list of stop words
python get_kept_stop_words.py $main_dir $cutoff

# Step 3: Get top words for each topic according to phis, mus, and tau2s
file_ave_params=${fit_dir}ave_params_gibbs_class.RData
file_kept_word_stems=${analysis_dir}kept_word_id_stems${cutoff}.txt
nwords_get=75
Rscript find_top_phi_words.R $fit_dir $analysis_dir $file_ave_params $file_kept_word_stems $nwords_get
Rscript find_top_mu_words.R $fit_dir $analysis_dir $file_ave_params $file_kept_word_stems $nwords_get
Rscript find_top_tau2_words.R $fit_dir $analysis_dir $file_ave_params $file_kept_word_stems $nwords_get


# Step 4: Get summary of tau2 distribution
Rscript get_tau2_sum.R $analysis_dir $file_ave_params $cutoff



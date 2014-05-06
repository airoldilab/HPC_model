# Master script to analyze output of model to understand word usage patterns
main_dir=/n/airoldifs2/lab/jbischof/reuters_output/
cutoff=500
fit_dir=${main_dir}mmm_fits/fit_train${cutoff}/
analysis_dir=${main_dir}mmm_analysis_out/
raw_data_dir=${main_dir}mmm_raw_data/

file_ave_params=${fit_dir}ave_params_gibbs.RData
file_kept_word_stems=${analysis_dir}kept_word_id_stems${cutoff}.txt

# Step 1: Extract list of words included in model
# Cut out tokens with less than three characters
python process_word_stems.py $main_dir $cutoff

# Step 2: Get total number of word tokens fit in model
python get_total_word_count.py $main_dir $cutoff

# Step 2: Get list of stop words
python get_kept_stop_words.py $main_dir $cutoff

# Step 3: Get top words for each topic according to phis, mus, and tau2s
nwords_get=75
weight_phi=0.5
Rscript find_top_phi_words.R $fit_dir $analysis_dir $file_ave_params $file_kept_word_stems $nwords_get
Rscript find_top_mu_words.R $fit_dir $analysis_dir $file_ave_params $file_kept_word_stems $nwords_get
Rscript find_top_tau2_words.R $fit_dir $analysis_dir $file_ave_params $file_kept_word_stems $nwords_get
Rscript find_top_sem_cont_words.R $fit_dir $analysis_dir $file_ave_params $file_kept_word_stems $nwords_get $weight_phi


# Step 4: Get comparison of top fe and top freq words for paper
n_use=14
Rscript get_fe_freq_comp.R ${analysis_dir} $n_use


# Step 5: Get comparison between regular and stop words
stop_reg_dir=${analysis_dir}stop_reg_plots/
if [ -d $stop_reg_dir ]
then
   rm -r $stop_reg_dir
fi
mkdir $stop_reg_dir

Rscript get_stop_reg_comp.R $analysis_dir $file_ave_params $cutoff $file_kept_word_stems


# Step 6: Get fe plots by topic
# Create output directory if doesn't already exist
fe_dir=${analysis_dir}fe_plots/
fe_full_dir=${fe_dir}full_plots/
fe_zoom_dir=${fe_dir}zoom_plots/
fe_joint_dir=${fe_dir}joint_plots/

# Is this a trial run?
test=0

if [ -d $fe_dir ]
then
   rm -r $fe_dir
fi
mkdir $fe_dir
mkdir $fe_full_dir
mkdir $fe_zoom_dir
mkdir $fe_joint_dir

Rscript get_fe_plots.R $analysis_dir $file_ave_params $file_kept_word_stems $test


# Step 7: Get tp plots by topic
# Create output directory if doesn't already exist
tp_dir=${analysis_dir}tau2_phi_plots/

if [ -d $tp_dir ]
then
   rm -r $tp_dir
fi
mkdir $tp_dir

Rscript get_tau2_phi_plots.R $analysis_dir $file_ave_params $file_kept_word_stems



# Step 8: Get tau2 plots by parent topic
# Create output directory if doesn't already exist
tau2_dist_dir=${analysis_dir}tau2_dist_plots/

if [ -d $tau2_dist_dir ]
then
   rm -r $tau2_dist_dir
fi
mkdir $tau2_dist_dir

Rscript get_tau2_level_dist.R $analysis_dir $file_ave_params $file_kept_word_stems
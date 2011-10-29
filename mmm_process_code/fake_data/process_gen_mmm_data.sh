# Script to coordinate creation of fake data to fit HPD model
main_dir=/n/airoldifs1/jbischof/reuters_output/
data_dir=${main_dir}mmm_raw_data/fake_data/

# Generate parameters for model
file_tab=${data_dir}mmm_topic_address_book.txt
file_true_params=${data_dir}mmm_true_params.RData
# Rscript gen_mmm_param_data.R $file_tab $file_true_params
# 
# # Generate data for each partition in LDA format
# Rscript gen_mmm_part_data.R $file_true_params $data_dir

# Create data in final format for each partition
for part in train test valid
do 
   out_dir_part=${data_dir}parsed_${part}_data/
   python process_fake_lda_data.py ${part}
done


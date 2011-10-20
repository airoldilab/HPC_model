import sys
sys.path.append("/n/home13/jbischof/reuters_prj/mmm_process_code/mmm_process_functions/")
from parse_lda_data import *
from initialize_theta_params import *

# Get partition and cutoff information from command line arguments
# 'partition' is the partition of the dataset (e.g., train, valid, test)
# 'cutoff' is the frequency cutoff for the word candidates (how many times need
# to see in entire corpus to consider for feature selection)
partition = sys.argv[1]

main_dir = "/n/airoldifs1/jbischof/reuters_output/"
data_dir = main_dir + "mmm_raw_data/fake_data/"
out_dir = data_dir + "parsed_" + str(partition) + "_data/"

infilename_lda = data_dir + "mmm_data_ldaformat_" + str(partition) + ".txt"
infilename_word_candidates = data_dir + "kept_word_ids.txt"
outfilename_doclength = out_dir + "doc_length_table.txt"
outfilename_doctopic = out_dir + "doc_topic_list.RData"
outfilename_docwc = out_dir + "doc_word_count_list.RData"
outfilename_featwc = out_dir + "feature_word_count_list.RData"
outfilename_docxi = out_dir + "doc_xi_list.RData"
outfilename_eta = out_dir + "eta_vec.txt"

parse_lda_data(infilename_lda, outfilename_doctopic, outfilename_doclength, \
infilename_word_candidates, outfilename_docwc, outfilename_featwc, \
outfilename_docxi, outfilename_eta, doctopic_dictname="doc.topic.list", \
docwc_dictname="doc.count.list", featwc_dictname="feature.count.list", \
docxi_dictname="doc.xi.list")


# Process theta
out_part_dir = data_dir + "parsed_" + str(partition) + "_data/"
infilename_tab = "mmm_topic_address_book_py.txt"
outfilename_theta = out_part_dir + "initialized_theta.txt"

# Initialize membership vectors (theta vectors) for each set of data
initialize_theta_params(infilename_lda,infilename_tab, \
outfilename_theta)

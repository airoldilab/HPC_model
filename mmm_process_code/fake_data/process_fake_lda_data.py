import sys
import os
sys.path.append("/n/home13/jbischof/reuters_prj/mmm_process_code/mmm_process_functions/")
from parse_lda_data import *
from initialize_tree_params import *
from initialize_theta_params import *

# Get partition and cutoff information from command line arguments
# 'partition' is the partition of the dataset (e.g., train, valid, test)
# 'cutoff' is the frequency cutoff for the word candidates (how many times need
# to see in entire corpus to consider for feature selection)
partition = sys.argv[1]

main_dir = "/n/airoldifs2/lab/jbischof/reuters_output/"
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

# Initialize tree parameters
infilename_selfeat = infilename_word_candidates
infilename_tab = "mmm_topic_address_book_py.txt"
outfilename_mu = out_dir + "initialized_mu.txt"
outfilename_mu_corpus = out_dir + "initialized_corpus_mu.txt"
outfilename_tau2 = out_dir + "initialized_tau2.txt"

# Process thetas
outfilename_theta = out_dir + "initialized_theta.txt"


# Functions

## Parse LDA data into format that R can read
parse_lda_data(infilename_lda, outfilename_doctopic, outfilename_doclength, \
infilename_word_candidates, outfilename_docwc, outfilename_featwc, \
outfilename_docxi, outfilename_eta, doctopic_dictname="doc.topic.list", \
docwc_dictname="doc.count.list", featwc_dictname="feature.count.list", \
docxi_dictname="doc.xi.list")

# Parameter initialization only makes sense for training set
if partition == "train":
   # Initialize tree parameters for all sets of data
   initialize_mmm_params(infilename_selfeat,infilename_tab,infilename_lda, \
   outfilename_mu,outfilename_mu_corpus,outfilename_tau2,L=1)
   
   # Process theta
   initialize_theta_params(infilename_lda,infilename_tab,outfilename_theta)
   
   # Get theta in sparse format
   source5 = "../mmm_process_functions/get_sparse_theta.R"
   cmd5 = "Rscript " + source5 + " " + out_dir
   check = os.system(cmd5)
   if not check == 0: raise Exception("Error in " + source5)
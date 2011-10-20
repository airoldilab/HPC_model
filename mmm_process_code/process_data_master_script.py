# Master script to coordinate data processing effort

# Import modules
import sys
sys.path.append("/n/home13/jbischof/reuters_prj/mmm_process_code/mmm_process_functions/")
from clean_lda_data import *
from get_word_candidates import *
from gen_train_test import *
from initialize_tree_params_from_simple import *
from initialize_theta_params import *

# Directories
main_dir = sys.argv[1]
raw_data_dir = sys.argv[2]
cutoff = int(sys.argv[3])
#main_dir = "/n/airoldifs1/jbischof/reuters_output/"
#raw_data_dir = main_dir + "mmm_raw_data/"
mu_fit_dir = main_dir + "comp_model_fits/mu_fits/"
tau2_fit_dir = main_dir + "comp_model_fits/tau2_fits/"


# Filenames

## Step 1
#infilename_lda = main_dir + "comp_reuters_ldaformat.txt"
#outfilename = main_dir + "reuters_ldaformat_clean.txt"

# Step 2
#raw_data_dir = main_dir + "mmm_raw_data/"
infile_wc = main_dir + "reuters_stemmed_wc.txt"
#cutoff = 500
outfile_cand = raw_data_dir + "reuters_mmm_kept_word_ids" + str(cutoff) + ".txt"

# Step 3
# First split
lda_filename1 = main_dir + "reuters_ldaformat_clean.txt"
lda_train_filename1 = raw_data_dir + "reuters_train_valid_ldaformat.txt"
lda_test_filename1 = raw_data_dir + "reuters_test_ldaformat.txt"
# Second split
lda_filename2 = raw_data_dir + "reuters_train_valid_ldaformat.txt"
lda_train_filename2 = raw_data_dir + "reuters_train_ldaformat.txt"
lda_test_filename2 = raw_data_dir + "reuters_valid_ldaformat.txt"

# Step 5
infilename_selfeat = raw_data_dir + "reuters_mmm_kept_word_ids" + str(cutoff) + ".txt"
infilename_tab = "topic_address_book_py.txt"
outfilename_mu = raw_data_dir + "initialized_mu" + str(cutoff) + ".txt"
outfilename_tau2 = raw_data_dir + "initialized_tau2" + str(cutoff) + ".txt"
outfilename_corpus_mu = raw_data_dir + "initialized_corpus_mu" + str(cutoff) + ".txt"

# Step 6
out_valid_dir = raw_data_dir + "parsed_valid_data" + str(cutoff) + "/"
out_train_dir = raw_data_dir + "parsed_train_data" + str(cutoff) + "/"
out_test_dir = raw_data_dir + "parsed_test_data" + str(cutoff) + "/"
infilename_lda_split1 = raw_data_dir + "reuters_valid_ldaformat.txt"
infilename_lda_split2 = raw_data_dir + "reuters_train_ldaformat.txt"
infilename_lda_split3 = raw_data_dir + "reuters_test_ldaformat.txt"
infilename_tab = "topic_address_book_py.txt"
#outfilename_theta_split1 = out_valid_dir + "initialized_theta.txt"
outfilename_theta_split2 = out_train_dir + "initialized_theta.txt"
#outfilename_theta_split3 = out_test_dir + "initialized_theta.txt"


# Functions

## Step 1
## Clean up original LDA file --- remove all document with no topics and 
## all 'C1511' assignments
#clean_lda_data(infilename_lda,outfilename)

# Step 2
# Determine pool of canididate words using raw frequencies
words_keep = get_word_cand(cutoff=cutoff,infile_name=infile_wc, \
outfile_name=outfile_cand)

# Step 3
# Split the cleaned LDA file into training, validation, and test sets
# First get split between train + valid and test sets
get_train_test(lda_filename1,lda_train_filename1,lda_test_filename1, \
nget_per100=3,debug=False)
# Then split between training and validation sets
get_train_test(lda_filename2,lda_train_filename2,lda_test_filename2, \
nget_per100=2,debug=False)

# Step 4
# Run all of the process_***_lda_data.py scripts using LSF

## Step 5
## Initialize tree parameters for all sets of data
#initialize_mmm_params(mu_fit_dir,tau2_fit_dir,infilename_selfeat,infilename_tab, \
#outfilename_mu,outfilename_tau2,outfilename_corpus_mu,debug=False)

## Step 6
## Initialize membership vectors (theta vectors) for training set
#initialize_theta_params(infilename_lda_split2,infilename_tab, \
#outfilename_theta_split2)
#source6 = "../mmm_process_functions/get_sparse_theta.R"
#cmd6 = "Rscript " + source6 + " " + out_train_dir
#check = os.system(cmd6)
#if not check == 0: raise Exception("Error in " + source6)

## This makes no sense on validation partitions
#initialize_theta_params(infilename_lda_split1,infilename_tab, \
#outfilename_theta_split1)
#initialize_theta_params(infilename_lda_split3,infilename_tab, \
#outfilename_theta_split3)

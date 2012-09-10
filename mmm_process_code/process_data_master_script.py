# Master script to coordinate data processing effort

# Import modules
import sys
import os
sys.path.append("/n/home13/jbischof/reuters_prj/mmm_process_code/mmm_process_functions/")
from get_word_candidates import *
from gen_train_test import *
from initialize_tree_params import *
from initialize_theta_params import *

# Directories
main_dir = sys.argv[1]
cutoff = int(sys.argv[2])
first_stage=int(sys.argv[3])
#cutoff = 500
#main_dir = "/n/airoldifs2/lab/jbischof/reuters_output/"
raw_data_dir = main_dir + "mmm_raw_data/"
out_train_dir = raw_data_dir + "parsed_train_data" + str(cutoff) + "/"

# Filenames

# Step 1
infile_wc = "/n/airoldifs2/lab/jbischof/reuters_output/reuters_stemmed_wc.txt"
outfile_cand = raw_data_dir + "reuters_mmm_kept_word_ids" + str(cutoff) + ".txt"

# Step 2
# First split
lda_filename1 = main_dir + "reuters_ldaformat_clean.txt"
lda_train_filename1 = raw_data_dir + "reuters_train_valid_ldaformat.txt"
lda_test_filename1 = raw_data_dir + "reuters_test_ldaformat.txt"
# Second split
lda_filename2 = raw_data_dir + "reuters_train_valid_ldaformat.txt"
lda_train_filename2 = raw_data_dir + "reuters_train_ldaformat.txt"
lda_valid_filename2 = raw_data_dir + "reuters_valid_ldaformat.txt"

# Step 4
infilename_selfeat = raw_data_dir + "reuters_mmm_kept_word_ids" + str(cutoff) + ".txt"
infilename_lda = lda_train_filename2
infilename_tab = "topic_address_book_py.txt"
outfilename_mu = out_train_dir + "initialized_mu" + str(cutoff) + ".txt"
outfilename_tau2 = out_train_dir + "initialized_tau2" + str(cutoff) + ".txt"
outfilename_mu_corpus = out_train_dir + "initialized_corpus_mu" + str(cutoff) + ".txt"

# Step 5
infilename_tab = "topic_address_book_py.txt"
outfilename_theta = out_train_dir + "initialized_theta.txt"


if first_stage:
   # Step 1
   # Determine pool of canididate words using raw frequencies
   words_keep = get_word_cand(cutoff=cutoff,infile_name=infile_wc, \
   outfile_name=outfile_cand)

   # Step 2
   # Split the cleaned LDA file into training, validation, and test sets
   # First get split between train + valid and test sets
   get_train_test(lda_filename1,lda_train_filename1,lda_test_filename1, \
   nget_per100=float(5)/3,debug=False)
   # Then split between training and validation sets
   get_train_test(lda_filename2,lda_valid_filename2,lda_train_filename2, \
   nget_per100=3,debug=False)

# Step 3
# Run all of the process_***_lda_data.py scripts using LSF

else:
   ## Step 1
   ## Determine pool of canididate words using raw frequencies
   #words_keep = get_word_cand(cutoff=cutoff,infile_name=infile_wc, \
   #outfile_name=outfile_cand)
   
   # Step 4
   # Initialize tree parameters for all sets of data
   initialize_mmm_params(infilename_selfeat,infilename_tab,infilename_lda, \
   outfilename_mu,outfilename_mu_corpus,outfilename_tau2,L=140)
   
   # Step 5
   # Initialize membership vectors (theta vectors) for training set
   initialize_theta_params(infilename_lda,infilename_tab, \
   outfilename_theta)
   source5 = "../mmm_process_functions/get_sparse_theta.R"
   cmd5 = "Rscript " + source5 + " " + out_train_dir
   check = os.system(cmd5)
   if not check == 0: raise Exception("Error in " + source5)
# Script to create training, validation, and test sets from Reuters data in 
# LDA format
# Goal is to get 33-33-33 split 

# Import modules
import sys
sys.path.append("mmm_process_functions/")
from gen_train_test import *

# Filenames
main_dir = "/n/airoldifs2/lab/jbischof/reuters_output/"
out_dir = "/n/airoldifs2/lab/jbischof/reuters_output/mmm_raw_data/"

# First split
lda_filename1 = main_dir + "reuters_ldaformat_clean.txt"
lda_train_filename1 = out_dir + "reuters_train_valid_ldaformat.txt"
lda_test_filename1 = out_dir + "reuters_test_ldaformat.txt"

# Second split
lda_filename2 = out_dir + "reuters_train_valid_ldaformat.txt"
lda_train_filename2 = out_dir + "reuters_train_ldaformat.txt"
lda_test_filename2 = out_dir + "reuters_valid_ldaformat.txt"

# First get split between train + valid and test sets
get_train_test(lda_filename1,lda_train_filename1,lda_test_filename1, \
nget_per100=3,debug=False)

# Then split between training and validation sets
get_train_test(lda_filename2,lda_train_filename2,lda_test_filename2, \
nget_per100=2,debug=False)
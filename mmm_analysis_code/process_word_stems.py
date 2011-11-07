# Script to analyze output of model to understand word usage patterns

import sys
from get_kept_word_stems import *

main_dir = sys.argv[1]
cutoff = sys.argv[2]

raw_data_dir = main_dir + "mmm_raw_data/"
analysis_dir = main_dir + "mmm_analysis_out/"

# Step 1 - Get stems for word ids used in model
infilename_kept_words = raw_data_dir + "reuters_mmm_kept_word_ids" + str(cutoff) +".txt"
infilename_stemmed_wc = main_dir + "reuters_stemmed_wc.txt"
outfile_kept_ids = analysis_dir + "kept_word_id_stems" + str(cutoff) +".txt"

get_kept_word_stems(infilename_kept_words,infilename_stemmed_wc, \
outfilename=outfile_kept_ids)


# Step 2

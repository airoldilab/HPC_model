import sys
sys.path.append("/n/home13/jbischof/reuters_prj/mmm_process_code/mmm_process_functions/")
from parse_lda_data import *

# Get partition and cutoff information from command line arguments
# 'partition' is the partition of the dataset (e.g., train, valid, test)
# 'cutoff' is the frequency cutoff for the word candidates (how many times need
# to see in entire corpus to consider for feature selection)
partition = sys.argv[1]
cutoff = sys.argv[2]
main_dir = sys.argv[3]

#main_dir = "/n/airoldifs2/lab/jbischof/reuters_output/"
data_dir = main_dir + "mmm_raw_data/"
out_dir = data_dir + "parsed_" + str(partition) + "_data" + str(cutoff) + "/"

infilename_lda = data_dir + "reuters_" + str(partition) + "_ldaformat.txt"
infilename_word_candidates = data_dir + "reuters_mmm_kept_word_ids" + str(cutoff) + ".txt"
outfilename_doclength = out_dir + "doc_length_table.txt"
outfilename_doctopic = out_dir + "doc_topic_list.RData"
outfilename_docwc = out_dir + "doc_word_count_list.RData"
outfilename_featwc = out_dir + "feature_word_count_list.RData"
outfilename_docxi = out_dir + "doc_xi_list.RData"
outfilename_eta = out_dir + "eta_vec.txt"

#parse_lda_data(infilename_lda,outfilename_doctopic,outfilename_doclength, \
#infilename_word_candidates, \
#outfilename_docwc,outfilename_featwc,doctopic_dictname="doc.topic.list", \
#docwc_dictname="doc.count.list",featwc_dictname="feature.count.list")

parse_lda_data(infilename_lda, outfilename_doctopic, outfilename_doclength, \
infilename_word_candidates, outfilename_docwc, outfilename_featwc, \
outfilename_docxi, outfilename_eta, doctopic_dictname="doc.topic.list", \
docwc_dictname="doc.count.list", featwc_dictname="feature.count.list", \
docxi_dictname="doc.xi.list")
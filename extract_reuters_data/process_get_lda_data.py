# Script to process output of extract_article_stemmed and process_topic_codes
# into lda format data

# Import modules
import sys
sys.path.append("/n/home13/jbischof/reuters_prj/extract_reuters_data/extract_functions/")
import string
from gen_lda_format import *
from gen_word_dict import *
#from get_topic_codes import *
#from gen_term_doc_topic_dict import *
from clean_lda_data import *

# Extract dictionary of all words --- create dictionary of unique
# token and assigns ID number
# Also generates count dictionary --- get marginal token counts
# Creates dictionary and also creates output file for analysis

# Filenames
dir_out = "/n/airoldifs2/lab/jbischof/reuters_output/"
#dir_out = "/n/airoldifs2/lab/jbischof/reuters_output/mmm_raw_data/"
infile_sax = dir_out + "reuters_stemmed_art_extract.txt"
infile_tdt = dir_out + "reuters_term_topic_codes.txt"
outfile_lda = dir_out + "reuters_ldaformat.txt"
outfile_swc = dir_out + "reuters_stemmed_wc.txt"
outfile_dl = dir_out + "reuters_doc_summaries.txt"
outfile_ds = dir_out + "doc_metadata.txt"

# Currently missing step from original topic codes to terminal topic codes

# Create dictionaries
# Dictionary of all unique words in corpus (with ids) and dictionary
# of their word counts
word_dict, count_dict = word_dict_gen(infile_sax)
# Dictionary of document ids and assigned topics 
doc_topic_dict = doc_topic_gen(infile_tdt)
# Dictionary of document ids and document lengths
length_dict = length_dict_gen(infile_sax,word_dict)
# Dictionary of document ids and all words counts (LDA format)
doc_dict = lda_format_gen(infile_sax,word_dict)

# Output dictionaries to file
lda_dict_out(infile_tdt,outfile_lda,doc_dict,length_dict,doc_topic_dict)
count_dict_out(outfile_swc,count_dict,word_dict)
doc_sum_out(outfile_ds,length_dict,doc_topic_dict)

infilename_lda = outfile_lda
outfilename_lda = dir_out + "reuters_ldaformat_clean.txt"
# Clean up original LDA file --- remove all document with no topics and 
# all 'C1511' assignments
clean_lda_data(infilename_lda,outfilename_lda)

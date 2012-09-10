# Script to get list of stop words amoung kept words

import string
import sys

main_dir = sys.argv[1]
cutoff = sys.argv[2]

raw_data_dir = main_dir + "mmm_raw_data/"
analysis_dir = main_dir + "mmm_analysis_out/"
infilename_kept_words = raw_data_dir + "reuters_mmm_kept_word_ids" + str(cutoff) +".txt"
infilename_stemmed_wc = "/n/airoldifs2/lab/jbischof/reuters_output/reuters_stemmed_wc.txt"
outfilename = analysis_dir + "kept_stop_words" + str(cutoff) +".txt"
infilename_stop_words = "stop_word_lists/SMART_nltk_merge_stem_stop.txt"


def get_kept_stop_words(infilename_kept_words,infilename_stemmed_wc, \
infilename_stop_words,outfilename):
   
   # Read in list of kept words
   infile_kept_words = open(infilename_kept_words,"r")
   kept_word_list = []
   for line in infile_kept_words:
      word_id, count = string.split(line.strip(),"\t")
      kept_word_list.append(word_id)
   infile_kept_words.close()
   kept_word_list = set(kept_word_list)
   
   # Read in list of word stems to word ids
   # Create dictionary of word_id to stem mappings
   word_stem_dict = {}
   infile_stemmed_wc = open(infilename_stemmed_wc,"r")
   for line in infile_stemmed_wc:
      word_id, word_stem, word_count = string.split(line.strip(),"\t")
      word_stem_dict[word_stem] = word_id
   
   # Get list of kept stop words and output to file
   outfile = open(outfilename,"w")
   infile_stop_words = open(infilename_stop_words,"r")
   kept_stop_word_list = []
   for line in infile_stop_words:
      word_stem = line.strip()
      if word_stem_dict.has_key(word_stem):
	 word_id = word_stem_dict[word_stem]
	 outstring = "%s\t%s\n" % (word_id, word_stem)
	 outfile.write(outstring)
   outfile.close()

if __name__ == "__main__":
   get_kept_stop_words(infilename_kept_words,infilename_stemmed_wc, \
   infilename_stop_words,outfilename)
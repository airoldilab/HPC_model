# Script to get total count of tokens in the dataset after feature selection

import string
import sys

main_dir = sys.argv[1]
cutoff = sys.argv[2]

#cutoff = 250
#main_dir = "/n/airoldifs2/lab/jbischof/reuters_output/"
raw_data_dir = main_dir + "mmm_raw_data/"
analysis_dir = main_dir + "mmm_analysis_out/"
infilename_kept_words = raw_data_dir + "reuters_mmm_kept_word_ids" + str(cutoff) +".txt"
infilename_stemmed_wc = main_dir + "reuters_stemmed_wc.txt"
outfilename = analysis_dir + "total_wc.txt"

def get_total_word_count(infilename_kept_words,outfilename):
   
   infile_kept_words = open(infilename_kept_words,"r")
   
   total_count = int(0)
   
   for line in infile_kept_words:
      word_id, count = string.split(line.strip(),"\t")
      total_count = total_count + int(count)
   
   outstring = "Total number of words is " + str(total_count)
   print outstring
   
   outfile = open(outfilename,"w")
   outfile.write(outstring)
   outfile.close()


if __name__ == "__main__":
   get_total_word_count(infilename_kept_words,outfilename)
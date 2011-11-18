# Script to translate my LDA-type data file for Reuters into something
# that LDA-C can read
# Need to do this for each partition

import string
import sys

main_dir = sys.argv[1]
cutoff = int(sys.argv[2])

# Filenames
#main_dir = "/n/airoldifs1/jbischof/reuters_output/"
raw_data_dir = main_dir + "mmm_raw_data/"
out_dir = raw_data_dir + "ldac_data/"
infilename_kept_words = raw_data_dir + "reuters_mmm_kept_word_ids" + str(cutoff) +".txt"

# Set up dictionaries of infiles and outfiles
part_list = ["train","valid","test"]
lda_infile_dict = {}
lda_outfile_dict = {}
for part in part_list:
   lda_infile_dict[part] = raw_data_dir + "reuters_" + part + "_ldaformat.txt"
   lda_outfile_dict[part] = out_dir + "reuters_" + part + "_ldac.dat"

outfilename_id_map = out_dir + "lda_word_id_map.txt"

def get_lda_data(lda_infile_dict,lda_outfile_dict,infilename_kept_words,outfilename_id_map):
   
   # Read in list of kept words
   new_id_dict = {}
   j = int(0)
   infile_kept_words = open(infilename_kept_words,"r")
   kept_word_list = []
   for line in infile_kept_words:
      word_id, count = string.split(line.strip(),"\t")
      kept_word_list.append(word_id)
      new_id_dict[word_id] = j
      j = j + 1
   infile_kept_words.close()
   kept_word_list_set = set(kept_word_list)
   
   # Write out dictionary of new ids
   outfile_id_map = open(outfilename_id_map,"w")
   for word_id in kept_word_list:
      new_id = new_id_dict[word_id]
      outstring = "%s\t%s\n" % (word_id, new_id)
      outfile_id_map.write(outstring)
   outfile_id_map.close()
   
   # Get list of partitions
   part_list = lda_infile_dict.keys()
   
   for part in part_list:
      # Load up my LDA file
      lda_file = open(lda_infile_dict[part],"r")
      # Ready outfile for writing
      outfile = open(lda_outfile_dict[part],"w")
      
      # Now go through my LDA file line by line and translate
      # into LDA-C format
      for line in lda_file:
	 line = line.strip()
	 doc_id, doc_topic_str, doc_length, word_str = string.split(line,"\t")
	 
	 # Cycle through word list and take out infrequent words
	 kept_word_str_list = []
	 word_str_list = string.split(word_str," ")
	 for item in word_str_list:
	    word_id, count = string.split(item,":")
	    if word_id in kept_word_list_set:
	       new_id = new_id_dict[word_id]
	       new_entry = "%s:%s" % (new_id, count)
	       kept_word_str_list.append(new_entry)
	 kept_word_str = string.join(kept_word_str_list," ")
	 
	 # Create LDA-C file
	 n_unique_tokens = str(len(kept_word_str_list))
	 outstring = "%s %s\n" % (n_unique_tokens, kept_word_str)
	 outfile.write(outstring)
	 
      outfile.close()


if __name__ == "__main__":
   get_lda_data(lda_infile_dict,lda_outfile_dict,infilename_kept_words,outfilename_id_map)
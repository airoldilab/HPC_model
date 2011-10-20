# Script load data on corpus word counts and choose a set
# of final candidates to analyze

import string
import math

def get_word_cand(cutoff,infile_name,outfile_name):
   
   # Ready file for reading and writing
   infile = open(infile_name,"r")
   outfile = open(outfile_name,"w")
   
   # Create dictionary of candidates
   word_cand_dict = {}
   
   # Process each line of word count input file
   for line in infile:
      # Separate input string by tabs
      line_list = string.split(line.strip(),"\t")
      word_id = line_list[0]
      word_count = line_list[2]
      
      if word_cand_dict.has_key(word_id):
	 word_cand_dict[word_id] = word_cand_dict[word_id] + int(word_count)
      else: word_cand_dict[word_id] = int(word_count)
   
   infile.close()
   
   # Go through dictionary and grab all words with counts greater than cutoff
   # Write all these words to outfile
   # Output string of ids of kept words
   words_keep = []
   for entry in word_cand_dict:
      count = word_cand_dict[entry]
      if count >= cutoff:
	 words_keep.append(entry)
	 # Setup output strings
	 prefix = "%s\t%s" % (entry, count)
	 suffix = "\n"
	 out_string = prefix + suffix
	 outfile.write(out_string)
   
   outfile.close()
   
   return words_keep

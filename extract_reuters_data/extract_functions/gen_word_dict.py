# -*- coding: utf-8 -*-
# Function to create dictionary of word counts

import string


def word_dict_gen(infile_name):
   # Create dictionary
   word_dict = {}
   count_dict = {}
   # ID index
   j = 1
   
   # Ready file for reading
   infile = open(infile_name,"r")
   
   # Process each line of input file
   for line in infile:
      # Separate input string by tabs
      line_list = string.split(line[0:len(line)-1],"\t")
      word = line_list[1]
      count = int(line_list[2])
      # Create id for word if not already in dictionary
      if word_dict.has_key(word):
	 count_dict[word] = count_dict[word] + count
      else:
	 word_id = j
	 j = j+1
	 word_dict[word] = j
	 count_dict[word] = count
   
   infile.close()
   
   return word_dict, count_dict


# Function to write count dictionary to file
def count_dict_out(outfile_name,count_dict,word_dict):
   # Ready file for writing
   outfile = open(outfile_name,"w")
   
   for entry in count_dict:
      count = count_dict[entry]
      word_id = word_dict[entry]
      out_string = "%s\t%s\t%s\n" % (word_id, entry, count)
      outfile.write(out_string)
      
   outfile.close()


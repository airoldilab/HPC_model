# Script to get the stems of kept word ids

import string

#main_dir = sys.argv[1]
#raw_data_dir = sys.argv[2]
#cutoff = sys.argv[3]

def get_kept_word_stems(infilename_kept_words,infilename_stemmed_wc, \
outfilename):
   
   # Read in list of kept words
   infile_kept_words = open(infilename_kept_words,"r")
   kept_word_list = []
   count_dict = {}
   for line in infile_kept_words:
      word_id, count = string.split(line.strip(),"\t")
      kept_word_list.append(word_id)
      count_dict[word_id] = count
   infile_kept_words.close()
      
   # Read in list of word stems to word ids
   # Create dictionary of word_id to stem mappings
   word_stem_dict = {}
   infile_stemmed_wc = open(infilename_stemmed_wc,"r")
   for line in infile_stemmed_wc:
      word_id, word_stem, word_count = string.split(line.strip(),"\t")
      word_stem_dict[word_id] = word_stem
   infile_stemmed_wc.close()
   
   # Output labeled version of kept word list
   # Only keep words with more than three characters
   outfile = open(outfilename,"w")
   for word_id in kept_word_list:
      word_stem = word_stem_dict[word_id]
      if len(word_stem) > 3:
	 count = count_dict[word_id]
	 outstring = "%s\t%s\t%s\n" % (word_id, word_stem, count)
	 outfile.write(outstring)
   outfile.close()

if __name__ == "__main__":
   get_kept_word_stems(infilename_kept_words,infilename_stemmed_wc, \
   outfilename)
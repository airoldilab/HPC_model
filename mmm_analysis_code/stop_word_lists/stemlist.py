# Script to porter stem any list of words
# Takes two arguments: name of infile and name of outfile

import sys
from nltk import PorterStemmer as porter
import string

# Open files with names given by user
infilename = sys.argv[1]
infile = open(infilename,"r")
outfilename = sys.argv[2]
outfile = open(outfilename,"w")
file_list = infile.readlines()
stem_list = []

# Read in and stem each word in file
for line in file_list:
   word = line.strip()
   stem_word = porter().stem(word)
   stem_list.append(stem_word + "\n")

# Keep only unique stems in list
out_list = list(set(stem_list))

# Write output to file
outfile.writelines(out_list)
infile.close()
outfile.close()
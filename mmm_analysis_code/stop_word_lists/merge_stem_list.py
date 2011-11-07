# Script to stem and merge two lists of words
# Give names of files as arguments to script
# Outfile will be last argument

import string
import sys
from nltk import PorterStemmer as porter

def nl_append(string):
   string_out = string + "\n"
   return string_out

# Open files with names given by user
infilenames = sys.argv[1:-1]
word_list = []

for filename in infilenames:
   infile = open(filename,"r")
   in_list = infile.readlines()
   in_list = map(string.strip,in_list)
   in_list = map(porter().stem,in_list)
   word_list.extend(in_list)
   infile.close()

# Keep only unique stems in list
out_list = list(set(word_list))
out_list.remove("")
out_list = map(nl_append,out_list)
out_list.sort()
print out_list[:50]

# Write output to file
outfilename = sys.argv[-1]
outfile = open(outfilename,"w")
outfile.writelines(out_list)
outfile.close()
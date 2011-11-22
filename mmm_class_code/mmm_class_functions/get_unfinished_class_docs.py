# Function to create list of docs for which have not done predictive inference

# Import modules
import string

import sys

filename_lda = sys.argv[1]
filename_comp = sys.argv[2]
outfilename = sys.argv[3]


def get_unfinished_class_docs(filename_lda,filename_comp,outfilename):
   
   file_lda = open(filename_lda,"r")
   file_comp = open(filename_comp,"r")
   outfile = open(outfilename,"w")
   
   # Go through file_lda line by line and grab doc_ids
   full_doc_dict = {}
   for line in file_lda:
      line_list = string.split(line.strip(),"\t")
      doc_id = line_list[0]
      full_doc_dict[doc_id] = 0
   file_lda.close()
   
   # Go through file_comp line by line and grab doc_ids
   # Mark those jobs that are done
   for line in file_comp:
      line_list = string.split(line.strip(),"\t")
      doc_id = line_list[0]
      full_doc_dict[doc_id] = 1
   file_comp.close()
   
   # Now write a file with all the unfinished docs
   n_left = 0
   for doc_id in full_doc_dict:
      if full_doc_dict[doc_id] == 0:
	 outstring = doc_id + "\n"
	 outfile.write(outstring)
	 n_left += 1
   outfile.close()
   
   print str(n_left) + " docs left to do"



if __name__ == "__main__":
   get_unfinished_class_docs(filename_lda,filename_comp,outfilename)

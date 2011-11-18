# Function to create list of docs for which have not done predictive inference

# Import modules
import string

def get_unfinished_class_docs(filename_comp,outfilename):
   
   file_comp = open(filename_comp,"r")
   outfile = open(outfilename,"w")
   
   # Go through file_comp line by line and grab doc_ids
   for line in file_comp:
      line_list = string.split(line.strip(),"\t")
      doc_id = line_list[0]
      outstring = doc_id + "\n"
      outfile.write(outstring)
   
   file_comp.close()
   outfile.close()

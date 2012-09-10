# Script to get topic membership for each document in format that can be
# used as GLM response

import string
import sys

#main_dir = sys.argv[1]

## Filenames
##main_dir = "/n/airoldifs2/lab/jbischof/reuters_output/"
#raw_data_dir = main_dir + "mmm_raw_data/"
#class_dir = main_dir + "mmm_class_out/"
#infilename_tab = "reuters_topic_address_book_py.txt"

## Data by partition
#part_list = ["train","valid","test"]
#part_file_dict = {}
#outfile_dict = {}
#for part in part_list:
   #infilename = raw_data_dir + "reuters_" + part + "_ldaformat.txt"
   #outfilename = raw_data_dir + "reuters_" + part + "_topic_membs.txt"
   #part_file_dict[part] = infilename
   #outfile_dict[part] = outfilename


def get_lda_data(part_file_dict,infilename_tab,part_list,outfile_dict):
   
   # Grab list of topics from topic address book (get standard order)
   topic_list = []
   infile_tab = open(infilename_tab,"r")
   for line in infile_tab:
      topic, level, parent = string.split(line.strip(),"\t")
      topic_list.append(topic)
   
   # Create header for doc_info file
   topic_list_str = string.join(topic_list,"\t")
   header = "%s\t%s\n" % ("doc_id",topic_list_str)
   
   # Cycle through partitions and create output files
   for part in part_list:
      
      # Get lda infile
      lda_part_filename = part_file_dict[part]
      lda_part_file = open(lda_part_filename,"r")
      
      # Open outfile file and write header
      outfilename = outfile_dict[part]
      outfile = open(outfilename,"w")
      outfile.write(header)
   
      # Now go through my LDA file line by line and get topic membs
      for line in lda_part_file:
	 line = line.strip()
	 doc_id, doc_topic_str, doc_length, word_str = string.split(line,"\t")
	 
	 # Want vector of indicators for whether topic k is in document d
	 doc_topic_list = string.split(doc_topic_str," ")
	 topic_indic_list = []
	 for topic in topic_list:
	    topic_indic = str(int(topic in doc_topic_list))
	    topic_indic_list.append(topic_indic)
	 topic_indic_str = string.join(topic_indic_list,"\t")
	 outstring_docdata = "%s\t%s\n" % (doc_id,topic_indic_str)
	 outfile.write(outstring_docdata)
      outfile.close()
      lda_part_file.close()


if __name__ == "__main__":
   get_lda_data(part_file_dict,infilename_tab,part_list,outfile_dict)
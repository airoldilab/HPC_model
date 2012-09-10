# Script create covariate matrix for logistic regression classifer
# from the train, valid, and test datasets

import string
import sys
import numpy as np

main_dir = sys.argv[1]
#pos = int(sys.argv[2]) - 1

# Directories
#main_dir = "/n/airoldifs2/lab/jbischof/reuters_output/"
raw_data_dir = main_dir + "mmm_raw_data/"
class_dir = main_dir + "mmm_class_out/"
ldac_dir = raw_data_dir + "ldac_data/"
liblinear_dir = raw_data_dir + "liblinear_data/"
out_dir = liblinear_dir

part_list = ["train","test","valid"]

# Filenames

infilename_tab = "reuters_topic_address_book_py.txt"

infile_part_topic_dict = {}
for part in part_list:
   infile_part_topic_dict[part] = raw_data_dir + "reuters_" + part + "_ldaformat.txt"


infile_part_ldac_dict = {}
for part in part_list:
   infile_part_ldac_dict[part] = ldac_dir + "reuters_" + part + "_ldac.dat"


outfile_stem_dict = {}
for part in part_list:
   outfile_stem_dict[part] = out_dir + "liblinear_data_" + part + "_"


def get_reuters_logit_covar(infile_part_topic_dict,infile_part_ldac_dict, \
infilename_tab,outfile_stem_dict):
   
   # Get list of partitions
   part_list = ["train","test","valid"]
   
   # Grab list of topics from topic address book (get standard order)
   topic_list = []
   infile_tab = open(infilename_tab,"r")
   for line in infile_tab:
      topic, level, parent = string.split(line.strip(),"\t")
      topic_list.append(topic)
   #topic_list = [topic_list[pos]]
   
   
   # Now for each partition get data for liblinear
   for part in part_list:
      
      # Get dictionary of document topics and list of documents in order
      part_doc_topic_dict = {}
      doc_id_list = []
      infile_part_topic = open(infile_part_topic_dict[part],"r")
      for line in infile_part_topic:
	 doc_id, doc_topic_str, doc_length, doc_wc_str = string.split(line.strip(),"\t")
	 doc_topic_list = string.split(doc_topic_str," ")
	 part_doc_topic_dict[doc_id] = doc_topic_list
	 doc_id_list.append(doc_id)
      
      # Now go through all the topics and create a data file
      for topic in topic_list:
	 
	 # Open up the output file
	 outfilename_part_topic = outfile_stem_dict[part] + topic + ".txt"
	 outfile_part_topic = open(outfilename_part_topic,"w")
      
	 # Go through lines of ldac file and sub in the correct label for each topic
	 infile_part_ldac = open(infile_part_ldac_dict[part],"r")
	 # Get index of the documents
	 j = int(0)
	 for line in infile_part_ldac:
	    # Load in line and delete first entry (count of unique terms)
	    doc_wc_list = string.split(line.strip()," ")
	    del doc_wc_list[0]
	    
	    # Now go though list and bump up indices of features by 1
	    # Also get order of list
	    doc_wc_list_fixed = []
	    doc_feat_id_list = []
	    for item in doc_wc_list:
	       word_id, count = string.split(item,":")
	       new_id = int(word_id) + 1
	       new_entry = "%s:%s" % (str(new_id),count)
	       doc_wc_list_fixed.append(new_entry)
	       doc_feat_id_list.append(new_id)
	    
	    # Get order of features
	    feat_id_order = list(np.argsort(doc_feat_id_list))
	    
	    # Now rebind list together in correct order
	    doc_wc_list_use = []
	    for k in feat_id_order:
	       doc_wc_list_use.append(doc_wc_list_fixed[k])
	    doc_wc_str = string.join(doc_wc_list_use," ")
	    
	    # Get id and topics of document
	    doc_id = doc_id_list[j]
	    j = j + 1
	    doc_topic_list = part_doc_topic_dict[doc_id]
	    
	    # Now create label for document
	    if topic in doc_topic_list:
	       doc_label = str(1)
	    else: doc_label = str(0)
	    
	    outstring = "%s %s\n" % (doc_label,doc_wc_str)
	    outfile_part_topic.write(outstring)
	 
	 outfile_part_topic.close()
	 infile_part_ldac.close()


if __name__ == "__main__":
   get_reuters_logit_covar(infile_part_topic_dict,infile_part_ldac_dict,infilename_tab, \
   outfile_stem_dict)
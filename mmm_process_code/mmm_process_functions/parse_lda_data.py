# Script to take raw data (in LDA format) and create RData objects that can be 
# fed into my fitting scripts

import string
import warnings
from translate_dict import *
import math

def parse_lda_data(infilename_lda,outfilename_doctopic,outfilename_doclength, \
   infilename_word_candidates,outfilename_docwc,outfilename_featwc, \
   outfilename_docxi, outfilename_eta, doctopic_dictname="doc.topic.list", \
   docwc_dictname="doc.count.list", featwc_dictname="feature.count.list", \
   docxi_dictname="doc.xi.list"):
   
   # Create dictionary for topic lists, doc lengths, and word counts
   doc_topic_dict = {}
   doc_length_dict = {}
   doc_word_count_dict = {}
   feature_word_count_dict = {}
   
   # Get list of word candidates
   infile_word_candidates = open(infilename_word_candidates,"r")
   kept_word_ids = []
   for line in infile_word_candidates:
      word_id = string.split(line.strip(),"\t")[0]
      kept_word_ids.append(word_id)
   
   # Count number of docs for topic log odds
   ndocs = int(0)
   topic_count_dict = {}
   
   # Read in LDA file line by line
   infile_lda = open(infilename_lda, "r")
   for line in infile_lda:
      ndocs = ndocs + int(1)
      line_list = string.split(line.strip(),"\t")
      # Deal with zero length documents
      doc_id, doc_topic_str, doc_length = line_list[0:3]
      if len(line_list) == 4: 
	 doc_wc_str = line_list[3]
      else: 
	 doc_wc_str = "1:0"
	 warnings.warn("One or more documents has zero word counts")
      #doc_id, doc_topic_str, doc_length, doc_wc_str = \
	 #string.split(line.strip(),"\t")
      
      # Parse document topics
      doc_tp_list = string.split(doc_topic_str," ")
      
      # Add topics to total count
      for topic in doc_tp_list:
	 if topic_count_dict.has_key(topic):
	    topic_count_dict[topic] = topic_count_dict[topic] + int(1)
	 else:
	    topic_count_dict[topic] = int(1)
      
      # Parse word counts
      doc_wc_dict = {}
      doc_wc_list = string.split(doc_wc_str," ")
      for item in doc_wc_list:
	 word_id, count = string.split(item,":")
	 # Only process kept word ids
	 if word_id in kept_word_ids:
	    doc_wc_dict[word_id] = int(float(count))
	    if feature_word_count_dict.has_key(word_id):
	       feature_word_count_dict[word_id][doc_id] = int(float(count))
	    else:
	       feature_word_count_dict[word_id] = {}
	       feature_word_count_dict[word_id][doc_id] = int(float(count))
      
      # Add entries to each dictionary
      doc_topic_dict[doc_id] = doc_tp_list
      doc_length_dict[doc_id] = doc_length
      doc_word_count_dict[doc_id] = doc_wc_dict
   
   ## Now create feature_word_count_dict from doc_word_count_dict
   #for doc_id in doc_word_count_dict:
      #doc_wc_list = doc_word_count_dict
   
   # Calculate log odds for each topic -- this is the initialized xi
   topic_log_odds_dict = {}
   #log_denom_p = math.log(float(ndocs))
   for topic in topic_count_dict:
      topic_log_prob = math.log(float(topic_count_dict[topic]))
      topic_log_comp = math.log(float(ndocs - topic_count_dict[topic]))
      topic_log_odds = topic_log_prob - topic_log_comp
      topic_log_odds_dict[topic] = topic_log_odds
      print topic
      print ndocs
      print topic_count_dict[topic]
   
   # Write log odds to file as initialized eta vec
   outfile_eta = open(outfilename_eta,"w")
   for topic in topic_log_odds_dict:
      topic_log_odds = topic_log_odds_dict[topic]
      outstring = "%s\t%s\n" % (topic,str(topic_log_odds))
      outfile_eta.write(outstring)
   outfile_eta.close()
   
   # Create dictionary of initialized xi for each document
   doc_xi_dict = {}
   for doc_id in doc_topic_dict:
      doc_xi_dict[doc_id] = {}
      for topic in doc_topic_dict[doc_id]:
	 doc_xi_dict[doc_id][topic] = topic_log_odds_dict[topic]
   
   # Write output to file
   # Doc length info is just a vector
   outfile_doclength = open(outfilename_doclength,"w")
   for doc_id in doc_length_dict:
      doc_length = doc_length_dict[doc_id]
      outstring = "%s\t%s\n" % (doc_id,doc_length)
      outfile_doclength.write(outstring)
   outfile_doclength.close()
   
   # But doc topic and word count info is a ragged array;
   # need to create list in R
   translate_dict(my_dict=doc_topic_dict,dict_name=doctopic_dictname, \
   filename=outfilename_doctopic,nested=False)
   translate_dict(my_dict=doc_word_count_dict,dict_name=docwc_dictname, \
   filename=outfilename_docwc,nested=True)
   translate_dict(my_dict=feature_word_count_dict,dict_name=featwc_dictname, \
   filename=outfilename_featwc,nested=True)
   translate_dict(my_dict=doc_xi_dict,dict_name=docxi_dictname, \
   filename=outfilename_docxi,nested=True)

if __name__=="__main__":
   parse_lda_data(infilename_lda,outfilename_doctopic,outfilename_doclength, \
   infilename_word_candidates,outfilename_docwc,outfilename_featwc, \
   outfilename_docxi, doctopic_dictname="doc.topic.list", \
   docwc_dictname="doc.count.list", featwc_dictname="feature.count.list", \
   docxi_dictname="doc.xi.list")
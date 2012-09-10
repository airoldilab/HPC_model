# Script to get tfidf scores and raw counts for all words in training set

import math
import sys
import string
import numpy as np
main_dir = sys.argv[1]
raw_data_dir = sys.argv[2]
infilename_stemmed_wc = "/n/airoldifs2/lab/jbischof/reuters_output/reuters_stemmed_wc.txt"
infilename_lda = raw_data_dir + "reuters_train_ldaformat.txt"
outfilename = raw_data_dir + "reuters_train_tfidf.txt"

def get_tfidf_scores(infilename_lda,infilename_stemmed_wc,outfilename):
   
   # Create dictionary of word_id to stem mappings
   word_stem_dict = {}
   infile_stemmed_wc = open(infilename_stemmed_wc,"r")
   for line in infile_stemmed_wc:
      word_id, word_stem, word_count = string.split(line.strip(),"\t")
      word_stem_dict[word_id] = word_stem
   
   
   # For reference, create dictionary of word counts
   word_count_dict = {}
   # Create dictionary for document frequency of terms
   doc_freq_dict = {}
   # Total number of documents
   ndocs = int(0)
   
   # Read in LDA file line by line
   infile_lda = open(infilename_lda, "r")
   for line in infile_lda:
      ndocs = ndocs + int(1)
      doc_id, doc_topic_str, doc_length, doc_wc_str = \
      string.split(line.strip(),"\t")
      
      # Parse word counts
      doc_wc_list = string.split(doc_wc_str," ")
      for item in doc_wc_list:
	 word_id, count = string.split(item,":")
	 if doc_freq_dict.has_key(word_id):
	    doc_freq_dict[word_id] = doc_freq_dict[word_id] + int(1)
	    word_count_dict[word_id] = word_count_dict[word_id] + int(count)
	 else: 
	    doc_freq_dict[word_id] = int(1)
	    word_count_dict[word_id] = int(count)
   infile_lda.close()
   
   
   # Now create tf-idf score
   tfidf_dict = {}
   
   # Read in LDA file line by line
   infile_lda = open(infilename_lda, "r")
   for line in infile_lda:
      doc_id, doc_topic_str, doc_length, doc_wc_str = \
      string.split(line.strip(),"\t")
      
      # Parse word counts
      doc_wc_list = string.split(doc_wc_str," ")
      for item in doc_wc_list:
	 word_id, count = string.split(item,":")
	 doc_freq = float(doc_freq_dict[word_id])
	 doc_tf_idf = (float(count)/float(doc_length)) * \
	 math.log(float(ndocs)/doc_freq)
	 if tfidf_dict.has_key(word_id):
	    tfidf_dict[word_id].append(doc_tf_idf)
	 else: tfidf_dict[word_id] = [doc_tf_idf]
   infile_lda.close()
   
   # Now go through tf-idf dict and get mean score across active documents
   word_id_list = []
   word_tfidf_list = []
   for word_id in tfidf_dict:
      doc_tfidf_list = tfidf_dict[word_id]
      word_tfidf_score = sum(doc_tfidf_list)/len(doc_tfidf_list)
      word_id_list.append(word_id)
      word_tfidf_list.append(word_tfidf_score)
      #tfidf_dict[word_id] = word_tfidf_score
   
   # Get order of word_ids based on tf-idf score
   word_id_order_list = list(np.argsort(word_tfidf_list))
   word_id_order_list.reverse()
   
   # Create output file
   outfile = open(outfilename,"w")
   
   for j in word_id_order_list:
      word_id = word_id_list[j]
      word_tfidf_score = word_tfidf_list[j]
      word_freq = word_count_dict[word_id]
      word_stem = word_stem_dict[word_id]
      outstring = "%s\t%s\t%s\t%s\n" % (word_id,word_stem,word_freq,word_tfidf_score)
      outfile.write(outstring)
   outfile.close()


if __name__ == "__main__":
   get_tfidf_scores(infilename_lda,infilename_stemmed_wc,outfilename)
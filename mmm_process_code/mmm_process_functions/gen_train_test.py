# Script to generate training and test set for classification exercise
# Implementing stratified sampling technique: want similar topic membership
# profile in test set as in training set
import random
import os
import string

def get_train_test(lda_filename,lda_train_filename,lda_test_filename, \
nget_per100=10,debug=False):
   
   # Step 1: Create two LDA files: a training set and a test set
   # Load in topic assignments for each document
   # Want entry in dictionary for each unique set of topics
   lda_file = open(lda_filename,"r")
   doc_topic_dict = {}
   for line in lda_file:
      line = line.strip()
      line_list = string.split(line,"\t")
      doc_id = int(line_list[0])
      topic_str = line_list[1]
      if doc_topic_dict.has_key(topic_str):
	 doc_topic_dict[topic_str].append(doc_id)
      else: doc_topic_dict[topic_str] = [doc_id]
   lda_file.close()
   
   # Now for each topic combination, select random nget% of docs to be test set
   test_doc_ids = []
   train_doc_ids = []
   for topic_str in doc_topic_dict:
      # Figure out how many units to sample
      doc_id_list = doc_topic_dict[topic_str]
      ndocs = len(doc_id_list)
      # Get number of units sample for sure
      divs_units = int(ndocs/float(nget_per100))
      # Figure how how many fractional units left
      rem_units = ndocs % nget_per100
      prob_extra = float(rem_units)/nget_per100
      unif_draw = random.random()
      get_extra = unif_draw < prob_extra
      if get_extra: nsample = divs_units + 1
      else: nsample = divs_units
      # Sanity check
      if debug:
	 print "topic_str =", topic_str
	 print "ndocs =", ndocs
	 print "divs_units =", divs_units
	 print "rem_units =", rem_units
	 print "prob_extra =", prob_extra
	 print "get_extra =", get_extra
	 print "nsample =", nsample
	 print ""
      # Then sample those many units
      sendto_test = random.sample(population=doc_id_list,k=nsample)
      sendto_train = [index for index in doc_id_list if index not in sendto_test]
      test_doc_ids.extend(sendto_test)
      train_doc_ids.extend(sendto_train)
   
   # Now need to load in LDA file and get those docs, then write out a
   # training and test set
   lda_file = open(lda_filename,"r")
   lda_train_file = open(lda_train_filename,"w")
   lda_test_file = open(lda_test_filename,"w")
   lda_file_list = lda_file.readlines()
   lda_train_set = []
   lda_test_set = []
   
   # Need to create mapping between doc_ids and lda_file index of doc
   doc_id_dict = {}
   for i in range(len(lda_file_list)):
      line = lda_file_list[i]
      line = line.strip()
      line_list = string.split(line,"\t")
      doc_id = int(line_list[0])
      doc_id_dict[doc_id] = i
   
   # Now can grab chosen training and test documents and put them
   # in separate files
   for doc_id in train_doc_ids:
      index = doc_id_dict[doc_id]
      lda_line = lda_file_list[index]
      lda_train_set.append(lda_line)
   
   for doc_id in test_doc_ids:
      index = doc_id_dict[doc_id]
      lda_line = lda_file_list[index]
      lda_test_set.append(lda_line)
   
   lda_train_file.writelines(lda_train_set)
   lda_train_file.close()
   lda_test_file.writelines(lda_test_set)
   lda_test_file.close()



if __name__ == "__main__":
   get_train_test(lda_filename,lda_train_filename,lda_test_filename)
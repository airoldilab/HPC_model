# Script to initialize mixed membership model fit

import string
import math
import copy

def initialize_mmm_params(infilename_selfeat,infilename_tab,infilename_lda, \
outfilename_mu,outfilename_mu_corpus,outfilename_tau2,L=int(1),debug=False):
   
   # Load up list of selected features
   sel_feat_list = []
   infile_selfeat = open(infilename_selfeat,"r")
   for line in infile_selfeat:
      line_list = string.split(line.strip(),"\t")
      word_id = line_list[0]
      sel_feat_list.append(word_id)
   infile_selfeat.close()
   
   # Grab list of topics from topic address book (get standard order)
   topic_list = []
   infile_tab = open(infilename_tab,"r")
   for line in infile_tab:
      topic, level, parent = string.split(line.strip(),"\t")
      topic_list.append(topic)
   ntopics = len(topic_list)
   if debug: print "topic_list: ", topic_list
   
   
   # Go through LDA file and get list of (1) total exposure for each topic, (2) count for each word in each topic. All this can be done assuming an equal membership model
   
   # Initialize topic_expose_dict
   topic_expose_dict = {}
   for topic in topic_list:
      topic_expose_dict[topic] = float(0)
   
   # Initialize feat_topic_count_dict
   feat_topic_count_dict = {}
   for word_id in sel_feat_list:
      feat_topic_count_dict[word_id] = {}
   
   # Read in LDA file line by line
   infile_lda = open(infilename_lda, "r")
   for line in infile_lda:
      doc_id, doc_topic_str, doc_length, doc_wc_str = string.split(line.strip(),"\t")
      
      doc_topic_list = string.split(doc_topic_str," ")
      doc_ntopics = len(doc_topic_list)
      doc_topic_prop = 1/float(doc_ntopics)
      
      # Divide up document exposure between active topics
      doc_topic_expose = doc_topic_prop*float(doc_length)
      for topic in doc_topic_list:
	 topic_expose_dict[topic] += doc_topic_expose
      
      # Parse word counts
      doc_wc_list = string.split(doc_wc_str," ")
      for item in doc_wc_list:
	 word_id, count = string.split(item,":")
	 # Only process selected features
	 if word_id in sel_feat_list:
	    # Figure out how to divide count for doc across active topics
	    doc_wt_count = float(count)*doc_topic_prop
	    
	    for topic in doc_topic_list:
	       if feat_topic_count_dict[word_id].has_key(topic):
		  feat_topic_count_dict[word_id][topic] += doc_wt_count
	       else: feat_topic_count_dict[word_id][topic] = doc_wt_count
   infile_lda.close()
   #print topic_expose_dict
   
   
   # Now use topic_expose_dict and feat_topic_count_dict to initialize rate parameters
   # Get smoothed estimator for each log rate in each topic
   smooth_mu_dict = {}
   for topic in topic_list:
      smooth_mu_dict[topic] = math.log(L) - math.log(topic_expose_dict[topic])
      
   # Dicitionary of initialized mus by topic
   mu_init_dict = {}
   # Initialize mus for every word-topic pair
   for word_id in sel_feat_list:
      # Start with smoothed rate for every topic
      mu_init_dict[word_id] = copy.deepcopy(smooth_mu_dict)
      # Grab dictionary of word topic counts
      feat_topic_count_list = copy.deepcopy(feat_topic_count_dict[word_id])
      # Then iterate through active topics and replace with more refined estimate
      for topic in feat_topic_count_list:
	 mu_init_dict[word_id][topic] = (math.log(L) + math.log(feat_topic_count_list[topic]) - math.log(topic_expose_dict[topic]))
   
   # Now use initialized mus to initialize corpus mus and tau2 parameters
   # For now, only initialize one tau2 per word
   mu_corpus_dict = {}
   tau2_corpus_dict = {}
   for word_id in sel_feat_list:
      # Get all rates for this feature
      word_topic_rates = mu_init_dict[word_id].values()
      mean_rate = sum(word_topic_rates)/float(ntopics)
      deviat_rate = [(rate - mean_rate)**2 for rate in word_topic_rates]
      var_rate = sum(deviat_rate)/float(ntopics - 1)
      mu_corpus_dict[word_id] = mean_rate
      tau2_corpus_dict[word_id] = var_rate
   
   # Now write parameter dictionaries out to file
   outfile_mu = open(outfilename_mu,"w")
   outfile_mu_corpus = open(outfilename_mu_corpus,"w")
   outfile_tau2 = open(outfilename_tau2,"w")
   
   # Write header with names of topics
   topic_header = string.join(topic_list,"\t")
   t_header = "\t%s\n" % (topic_header)
   outfile_mu.write(t_header)
   
   # Write out parameter values in correct order
   for word_id in sel_feat_list:
      word_mu_list = []
      for topic in topic_list:
	 word_topic_mu = mu_init_dict[word_id][topic]
	 word_mu_list.append(str(word_topic_mu))
      word_mu_str = string.join(word_mu_list,"\t")
      outstring_mu = "%s\t%s\n" % (word_id,word_mu_str)
      outfile_mu.write(outstring_mu)
      del word_mu_list
   outfile_mu.close()
   
   # Write corpus level mus and tau2s to output file
   outfile_mu_corpus = open(outfilename_mu_corpus,"w")
   outfile_tau2 = open(outfilename_tau2,"w")
   for word_id in sel_feat_list:
      outstring = "%s\t%f\n" % (word_id, mu_corpus_dict[word_id])
      outfile_mu_corpus.write(outstring)
      outstring = "%s\t%f\n" % (word_id, tau2_corpus_dict[word_id])
      outfile_tau2.write(outstring)
   outfile_mu_corpus.close()
   outfile_tau2.close()

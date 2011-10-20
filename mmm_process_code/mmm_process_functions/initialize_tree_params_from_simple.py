# Script to initialize mixed membership model fit using fits from full membership model

import string

## Filenames
#main_dir = "/n/airoldifs1/jbischof/reuters_output/"
#data_dir = main_dir + "mmm_raw_data/"
#mu_fit_dir = main_dir + "comp_model_fits/mu_fits/"
#tau2_fit_dir = main_dir + "comp_model_fits/tau2_fits/"
#infilename_selfeat = data_dir + "reuters_mmm_kept_word_ids.txt"
#infilename_tab = "topic_address_book_py.txt"
#outfilename_mu = data_dir + "initialized_mu.txt"
#outfilename_tau2 = data_dir + "initialized_tau2.txt"
#outfilename_corpus_mu = data_dir + "initialized_corpus_mu.txt"

def initialize_mmm_params(mu_fit_dir,tau2_fit_dir,infilename_selfeat,infilename_tab, \
outfilename_mu,outfilename_tau2,outfilename_corpus_mu,debug=False):
   
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
   parent_list = []
   infile_tab = open(infilename_tab,"r")
   for line in infile_tab:
      topic, level, parent = string.split(line.strip(),"\t")
      topic_list.append(topic)
      if parent not in parent_list: parent_list.append(parent)
   if debug: print "topic_list: ", topic_list
   if debug: print "parent_list: ", parent_list
   
   
   # Create dictionary of log rates for selected features
   feat_mu_dict = {}
   mu_corpus_dict = {}
   for feat_id in sel_feat_list:
      mu_dict = {}
      infilename_mu = mu_fit_dir + "mu_" + feat_id + ".txt"
      infile_mu = open(infilename_mu,"r").readlines()
      if infile_mu[0][0:3] == '"x"':
	 del infile_mu[0]
      for line in infile_mu:
	 topic, log_mu = string.split(line.strip()," ")
	 if topic[0] == '"':
	    topic = topic[1:-1]
	 if topic=="CORPUS":
	    mu_corpus_dict[feat_id] = log_mu
	 else: mu_dict[topic] = log_mu
      feat_mu_dict[feat_id] = mu_dict
      #if debug: 
	 #print "feat_id: ", feat_id
	 #print "mu_dict: ", mu_dict
	 #print
   
   # Create dictionary of discrimination parameters for selected features
   feat_tau2_dict = {}
   for feat_id in sel_feat_list:
      tau2_dict = {}
      infilename_tau2 = tau2_fit_dir + "tau2_" + feat_id + ".txt"
      infile_tau2 = open(infilename_tau2,"r").readlines()
      if infile_tau2[0][0:3] == '"x"':
	 del infile_tau2[0]
      for line in infile_tau2:
	 topic, tau2 = string.split(line.strip()," ")
	 if topic[0] == '"':
	    topic = topic[1:-1]
	 tau2_dict[topic] = tau2
	 feat_tau2_dict[feat_id] = tau2_dict
   
   # Now write parameter dictionaries out to file
   outfile_mu = open(outfilename_mu,"w")
   outfile_tau2 = open(outfilename_tau2,"w")
   
   # Write header with names of topics
   topic_header = string.join(topic_list,"\t")
   t_header = "\t%s\n" % (topic_header)
   parent_header = string.join(parent_list,"\t")
   p_header = "\t%s\n" % (parent_header)
   outfile_mu.write(t_header)
   outfile_tau2.write(p_header)
   
   # Write out parameter values in correct order
   for feat_id in sel_feat_list:
      feat_mu_list = []
      feat_tau2_list = []
      for topic in topic_list:
	 feat_topic_mu = feat_mu_dict[feat_id][topic]
	 feat_mu_list.append(feat_topic_mu)
      for topic in parent_list:
	 feat_topic_tau2 = feat_tau2_dict[feat_id][topic]
	 feat_tau2_list.append(feat_topic_tau2)
      feat_mu_str = string.join(feat_mu_list,"\t")
      feat_tau2_str = string.join(feat_tau2_list,"\t")
      outstring_mu = "%s\t%s\n" % (feat_id,feat_mu_str)
      outstring_tau2 = "%s\t%s\n" % (feat_id,feat_tau2_str)
      outfile_mu.write(outstring_mu)
      outfile_tau2.write(outstring_tau2)
   outfile_mu.close()
   outfile_tau2.close()
   
   # Write corpus level mus out to output file
   outfile_corpus_mu = open(outfilename_corpus_mu,"w")
   for feat_id in mu_corpus_dict:
      log_mu = mu_corpus_dict[feat_id]
      outstring = "%s\t%s\n" % (feat_id, log_mu)
      outfile_corpus_mu.write(outstring)
   outfile_corpus_mu.close()


if __name__ == "__main__":
   initialize_mmm_params(mu_fit_dir,tau2_fit_dir,infilename_selfeat,infilename_tab, \
   outfilename_mu,outfilename_tau2,outfilename_corpus_mu,debug=False)
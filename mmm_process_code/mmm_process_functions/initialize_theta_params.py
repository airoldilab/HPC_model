# Script to initialize membership vectors as uniform vectors over the 
# active topics

import string

# Set up directories
#main_dir = "/n/airoldifs2/lab/jbischof/reuters_output/"
#data_dir = main_dir + "mmm_raw_data/"
#infilename_lda = data_dir + "reuters_valid_ldaformat.txt"
#infilename_tab = "topic_address_book_py.txt"
#outfilename = data_dir + "valid_initial_theta.txt"
#outfilename_nonedocs = data_dir + "valid_none_docs.txt"

def initialize_theta_params(infilename_lda,infilename_tab,outfilename, \
outfilename_nonedocs=None,debug=False):
   
   # Figure out if files need to be cleaned
   clean = outfilename_nonedocs != None
   
   # Grab list of topics from topic address book (get standard order)
   topic_list = []
   infile_tab = open(infilename_tab,"r")
   for line in infile_tab:
      topic, level, parent = string.split(line.strip(),"\t")
      topic_list.append(topic)
   if debug: print "topic_list: ", topic_list
   
   # Get total number of topics
   n_topics = len(topic_list)
   
   # Read in LDA format file and create dictionary of document topics
   infile_lda = open(infilename_lda,"r")
   doc_topic_dict = {}
   if clean: none_docs = []
   for line in infile_lda:
      line_list = string.split(line.strip(),"\t")
      doc_id = line_list[0]
      doc_topic_str = line_list[1]
      doc_topic_list = string.split(doc_topic_str," ")
      if clean:
	 # Check for 'C1511' topic
	 if "C1511" in doc_topic_list: doc_topic_list.remove("C1511")
	 if "None" in doc_topic_list or len(doc_topic_list) == 0:
	    none_docs.append(doc_id)
	    continue
      doc_topic_dict[doc_id] = doc_topic_list
   infile_lda.close()
   
   # Create output file 
   outfile = open(outfilename,"w")
   
   # Write header with names of topics
   topic_header = string.join(topic_list,"\t")
   header = "\t%s\n" % (topic_header)
   outfile.write(header)

   # Create membership vector for every document
   for doc_id in doc_topic_dict:
      doc_topic_list = doc_topic_dict[doc_id]
      n_doc_topics = len(doc_topic_list)
      #print doc_topic_list
      #print n_doc_topics
      doc_theta_active = float(1)/n_doc_topics
      # First create empty vector with all zeros
      doc_mem_list = [0]*n_topics
      # Then fill in 1/K_A theta for every active topic
      for topic in doc_topic_list:
	 # Find out position of topic
	 if topic=='': print doc_id
	 index_topic = topic_list.index(topic)
	 # Fill in position in memb vec
	 doc_mem_list[index_topic] = doc_theta_active
      # Write vector to output file
      doc_mem_list = map(str,doc_mem_list)
      doc_mem_str = string.join(doc_mem_list,"\t")
      outstring = "%s\t%s\n" % (doc_id,doc_mem_str)
      outfile.write(outstring)
   outfile.close()
   
   if clean:
      # Print out list of problematic docs
      outfile_nonedocs = open(outfilename_nonedocs,"w")
      for doc_id in none_docs:
	 outfile_nonedocs.write(doc_id + "\n")
      outfile_nonedocs.close()


if __name__ == "__main__":
   initialize_theta_params(infilename_lda,infilename_tab,outfilename, \
   outfilename_nonedocs=None,debug=False)
import string
import re

# Function to create doc length dictionary
def gen_term_doc_topic_dict(infile_name,outfile_name):
   # Ready file for reading
   infile = open(infile_name,"r")
   # Ready file for writing
   outfile = open(outfile_name,"w")
   
   # Create list of non-terminal nodes
   nt_nodes = ["C15","C17","C18","C31","C33","C41","CCAT","E12","E13","E14","E21","E31","E41","E51","ECAT","G11","G13","G15","GCAT","M13","M14","MCAT"]
   
   # Process each line of input file
   for line in infile:
      # Separate input string by tabs, get rid of newline ch
      line_list = string.split(line[0:len(line)-1],"\t")
      
      # Unpack list
      doc_id = line_list[0]
      topic_str = line_list[2]
      topic_list = string.split(topic_str," ")
      
      # Now for every main branch in which article has membership,
      # want to know if membership ends at terminal node or 
      # non-terminal node
      # Want list of non-terminal and terminal topics in document
      nterm_topics = []
      term_topics = []
      for topic in topic_list:
	 if topic in nt_nodes: nterm_topics.append(topic)
	 else: term_topics.append(topic)
      # Now for every non-terminal topic, want to know if child
      # in list of topics
      for topic in nterm_topics:
	 # First assume that 'po' topic
	 po_ind = 1
	 # First deal with mbranch topics
	 if len(topic) >= 4 and topic[1:4] == "CAT": 
	    mbranch = topic[0]
	    for cand_topic in topic_list:
	       child_match_num = re.match(mbranch + "[0-9]{2,}",cand_topic)
	       if len(cand_topic) >= 4 and cand_topic[1:4] == "CAT":
		  child_match_txt = None
	       else: child_match_txt = re.match(mbranch + "[A-Z]{1,}",cand_topic)
	       # If see a child, set indicator to '0'
	       if child_match_num != None or child_match_txt != None: po_ind = 0
	 # Now deal with second level topics
	 elif re.match("[A-Z][0-9]{2,2}",topic) != None: 
	    mbranch = topic[0]
	    for cand_topic in topic_list:
	       child_match = re.match(topic + "[0-9]{1}",cand_topic)
	       # If see a child, set indicator to '0'
	       if child_match != None: po_ind = 0
	 # If topic not caught by either if statement, print error
	 else: 
	    print "Could not match topic", topic, "in 'po' loop"
	    po_ind = 0
	 # If topic a non-terminal node but none of its children present,
	 # add to the list of terminal topics
	 if po_ind == 1: term_topics.append(topic)
      
      # Write list of terminal topics to outfile
      n_topics = len(term_topics)
      term_topic_str = string.join(term_topics)
      prefix = "%s\t%s\t%s" % (doc_id, n_topics, term_topic_str)
      suffix = "\n"
      out_string = prefix + suffix
      outfile.write(out_string)
   
   infile.close()
   outfile.close()


infile_name = "/n/airoldifs2/lab/jbischof/reuters_output/reuters_topic_codes.txt"
outfile_name = "/n/airoldifs2/lab/jbischof/reuters_output/reuters_term_topic_codes.txt"
gen_term_doc_topic_dict(infile_name,outfile_name)

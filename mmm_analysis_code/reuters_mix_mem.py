# Script to extract mixed membership probabilities in Reuters corpus

import string
import re
import math

def normalize_dict(dict_in,n):
   for entry in dict_in:
      prob = float(dict_in[entry])/float(n)
      dict_in[entry] = "%0.4f" % (prob)
   return(dict_in)


# Function to create doc length dictionary
def mem_dicts_gen(infile_name):
   # Ready file for reading
   infile = open(infile_name,"r")
   
   # Create dictionary for mixed membership and
   # number of topics
   mixm_dict = {"cl":0,"cb2":0,"cb3":0,"any":0}
   nmem_dict = {}
   ndocs = 0
   
   # Create list of non-terminal nodes and 'po' dictionary 
   nt_nodes = ["C15","C17","C18","C31","C33","C41","CCAT","E12","E13","E14","E21","E31","E41","E51","ECAT","G11","G13","G15","GCAT","M13","M14","MCAT"]
   po_dict = {}
   po_nomix_dict = {}
   topic_mem_dict = {}
   topic_cl_dict = {}
   topic_cb2_dict = {}
   topic_cb3_dict = {}
   topic_any_dict = {}
   for item in nt_nodes: 
      po_dict[item] = 0
      po_nomix_dict[item] = 0
   
   # Process each line of input file
   for line in infile:
      # Separate input string by tabs, get rid of newline ch
      line_list = string.split(line.strip(),"\t")
      
      # Unpack list
      doc_id = line_list[0]
      n_topics = int(line_list[1])
      topic_str = line_list[2]
      topic_list = string.split(topic_str," ")
      mbranch_count = [0,0,0,0]
      # Main branches list
      mbranch_lookup = ["C","E","G","M"]
      
      if topic_str == "None": continue
      
      else:
	 ndocs = ndocs + 1
	 
	 # Clean up topic_list
	 if "C1511" in topic_list:
	    topic_list.remove("C1511")
	    # Make sure parent is there
	    if not "C151" in topic_list:
	       topic_list.append("C151")
	 
	 # First get main branch memberships
	 for item in topic_list:
	    mbranch_match = re.match("[A-Z]",item)
	    mbranch = mbranch_match.group()
	    if mbranch in mbranch_lookup:
	       pos = mbranch_lookup.index(mbranch)
	       mbranch_count[pos] = mbranch_count[pos] + 1
	 
	 # Mixed membership at highest level
	 nzero = len([x for x in mbranch_count if x==0])
	 if nzero < 3: 
	    mixm_dict["cl"] = mixm_dict["cl"] + 1
	    mixm_cl = 1
	 else: mixm_cl = 0
	 
	 # Now determine if mixed membership below first level
	 # Main branches with more than one topic listed
	 # In regexps, {n} means *at least* 'n' occurrences 
	 pos_mmbl = []
	 for index, count in enumerate(mbranch_count):
	    if count > 1: pos_mmbl.append(index)
	 mmb_cb = [0,0]
	 for pos in pos_mmbl:
	    mbranch_name = mbranch_lookup[pos]
	    topics_mbranch = [topic for topic in topic_list if topic[0]==mbranch_name and topic[1:4] != "CAT" and topic!= None]
	    level_count = [0,0]
	    for topic in topics_mbranch:
	       L2_match = re.match("[A-Z]{4,5}",topic)
	       L2n_match = re.search("[A-Z][0-9]{2}$",topic)
	       L3n_match = re.search("[A-Z][0-9]{3}$",topic)
	       if L2_match != None: level_count[0] += 1
	       if L2n_match != None: level_count[0] += 1
	       if L3n_match != None: level_count[1] += 1
	    if level_count[0]>1: mmb_cb[0] = 1
	    if level_count[1]>1: mmb_cb[1] = 1
	 if mmb_cb[0]>=1: 
	    mixm_dict["cb2"] += 1
	    mixm_cb2 = 1
	 else: mixm_cb2 = 0
	 if mmb_cb[1]>=1: 
	    mixm_dict["cb3"] += 1
	    mixm_cb3 = 1
	 else: mixm_cb3 = 0
	 if mixm_cb2 or mixm_cb3 or mixm_cl:
	    mixm_dict["any"] += 1
	    mixm_any = 1
	 else: mixm_any = 0
	 
	 # Now for every main branch in which article has membership,
	 # want to know if membership ends at terminal node or 
	 # non-terminal node
	 # Want list of non-terminal topics in document
	 nterm_topics = []
	 for topic in topic_list:
	    if topic in nt_nodes: nterm_topics.append(topic)
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
	    
	    if po_ind == 1: po_dict[topic] = po_dict[topic] + 1
	    if po_ind == 1 and mixm_any == 0: 
	       po_nomix_dict[topic] = po_nomix_dict[topic] + 1
	 
	 # Create entry in topic_mem_dict if needed, else just update entry
	 
	 for topic in topic_list:
	    if topic_mem_dict.has_key(topic):
	       topic_mem_dict[topic] += 1
	       topic_cl_dict[topic] += mixm_cl
	       topic_cb2_dict[topic] += mixm_cb2
	       topic_cb3_dict[topic] += mixm_cb3
	       topic_any_dict[topic] += mixm_any
	    else: 
	       topic_mem_dict[topic] = 1
	       topic_cl_dict[topic] = mixm_cl
	       topic_cb2_dict[topic] = mixm_cb2
	       topic_cb3_dict[topic] = mixm_cb3
	       topic_any_dict[topic] = mixm_any
	 
	 # Create entry in nmem_dict if needed, else just update entry
	 if nmem_dict.has_key(n_topics):
	    nmem_dict[n_topics] += 1
	 else: nmem_dict[n_topics] = 1
   
   infile.close()
   
   return ndocs, mixm_dict, nmem_dict, po_dict, po_nomix_dict, topic_mem_dict, topic_cl_dict, topic_cb2_dict, topic_cb3_dict, topic_any_dict


# Function to write mixm dictionary to file
def mixm_dict_out(outfile_name,mixm_dict,ndocs):
   # Ready file for writing
   outfile = open(outfile_name,"w")
   
   for entry in mixm_dict:
      prop_mix = mixm_dict[entry]/float(ndocs)
      out_string = "%s\t%0.3f\n" % (entry, prop_mix)
      outfile.write(out_string)
   
   outfile.close()
   out_docs = open("ndocs.txt","w")
   out_docs.write(str(ndocs)+"\n")
   out_docs.close()


# Function to write nmem dictionary to file
def nmem_dict_out(outfile_name,nmem_dict):
   # Ready file for writing
   outfile = open(outfile_name,"w")
   
   for entry in nmem_dict:
      nmem = nmem_dict[entry]
      out_string = "%s\t%s\n" % (entry, nmem)
      outfile.write(out_string)
   
   outfile.close()


# Function to write po dictionary to file
def po_dict_out(outfile_name,po_dict,po_nomix_dict):
   # Ready file for writing
   outfile = open(outfile_name,"w")
   
   for entry in po_dict:
      po = po_dict[entry]
      po_nomix = po_nomix_dict[entry]
      out_string = "%s\t%s\t%s\n" % (entry, po, po_nomix)
      outfile.write(out_string)
   
   outfile.close()

# Function to write topic_mem_dict to file
def topic_mem_dicts_out(outfile_name,topic_mem_dict,topic_any_dict,topic_cl_dict,topic_cb2_dict,topic_cb3_dict,topic_code_file):
   
   # Load up topic names
   topic_name_dict = {}
   topic_code_file = open(topic_code_file,"r")
   topic_code_list = []
   for line in topic_code_file:
      #topic_code, topic_name = string.split(line.strip(),"\t")
      topic_code, topic_name = line.strip().split("\t")
      topic_name_dict[topic_code] = topic_name
      topic_code_list.append(topic_code)
   
   # Ready file for writing
   outfile = open(outfile_name,"w")
   header = "%s\t%s\t%s\t%s\t%s\t%s\t%s\n" % ("topic_code", "topic", "tot_mem","any","cl","cb2","cb3")
   outfile.write(header)
   
   for topic_code in topic_code_list:
      if topic_mem_dict.has_key(topic_code):
	 topic_name = topic_name_dict[topic_code]
	 tot_mem = int(topic_mem_dict[topic_code])
	 tot_any = 100*topic_any_dict[topic_code]/float(tot_mem)
	 tot_cl = 100*topic_cl_dict[topic_code]/float(tot_mem)
	 tot_cb2 = 100*topic_cb2_dict[topic_code]/float(tot_mem)
	 tot_cb3 = 100*topic_cb3_dict[topic_code]/float(tot_mem)
	 out_string = "%s\t%s\t%d\t%0.1f\t%0.1f\t%0.1f\t%0.1f\n" % (topic_code, topic_name, tot_mem, tot_any, tot_cl, tot_cb2, tot_cb3)
	 outfile.write(out_string)
   
   outfile.close()

topic_code_file = "/n/home13/jbischof/reuters_prj/mmm_analysis_code/topic_codes.txt"
main_dir = "/n/airoldifs1/jbischof/reuters_output/"
out_dir = main_dir + "mmm_analysis_out/"
infile_name = main_dir + "reuters_term_topic_codes.txt"
outfile_mixm = out_dir + "mix_mem_tab.txt"
outfile_nmem = out_dir + "nmem_tab.txt"
outfile_po = out_dir + "po_topic_obs.txt"
outfile_tm = out_dir + "topic_membs.txt"
ndocs, mixm_dict, nmem_dict, po_dict, po_nomix_dict, topic_mem_dict, topic_cl_dict, topic_cb2_dict, topic_cb3_dict, topic_any_dict = mem_dicts_gen(infile_name)
mixm_dict_out(outfile_mixm,mixm_dict,ndocs)
nmem_dict_out(outfile_nmem,nmem_dict)
po_dict_out(outfile_po,po_dict,po_nomix_dict)
topic_mem_dicts_out(outfile_tm,topic_mem_dict,topic_any_dict,topic_cl_dict,topic_cb2_dict,topic_cb3_dict,topic_code_file)




# Program to load in data, create a dictionary, 
# and output data in LDA-type format

from gen_word_dict import *

# Function to create doc length dictionary
def length_dict_gen(infile_name,word_dict):
   # Ready file for reading
   infile = open(infile_name,"r")
   
   # Create dictionary for doc lengths
   length_dict = {}
   
   # Process each line of input file
   for line in infile:
      # Separate input string by tabs
      line_list = string.split(line.strip(),"\t")
      
      # Unpack list
      doc_id = str(line_list[0])
      count = line_list[2]
      
      # Create entry in length_dict if needed, else just update entry
      if length_dict.has_key(doc_id):
	 length_dict[doc_id] = length_dict[doc_id] + int(count)
      else: length_dict[doc_id] = int(count)
   
   infile.close()
   
   return length_dict


# Function to create doc_topic dictionary
def doc_topic_gen(infile_name):
   # Ready file for reading
   infile = open(infile_name,"r")
   
   # Create dictionary for doc topics
   doc_topic_dict = {}
   
   # Process each line of input file
   for line in infile:
      # Separate input string by tabs
      line_list = string.split(line.strip(),"\t")
      
      # Unpack list
      doc_id = str(line_list[0])
      topic_str = line_list[2]
      
      # Create entry in doc_topic_dict if needed, else just update entry
      # (this should never happen)
      if doc_topic_dict.has_key(doc_id):
	 doc_topic_dict[doc_id] = doc_topic_dict[doc_id] + " " + topic_str
      else: doc_topic_dict[doc_id] = topic_str
	 
   infile.close()
	 
   return doc_topic_dict


# Function to create lda dictionary
def lda_format_gen(infile_name,word_dict):
   
   # Ready file for reading
   infile = open(infile_name,"r")
   
   # Create dictionary for documents
   doc_dict = {}
   
   # Process each line of input file
   for line in infile:
      # Separate input string by tabs
      line_list = string.split(line.strip(),"\t")
      
      # Unpack list
      doc_id = str(line_list[0])
      word = line_list[1]
      count = line_list[2]
      
      # Get word id
      word_id = word_dict.get(word,"????")
      
      # Create doc_dict entry for word
      word_entry = str(word_id) + ":" + str(count)
      
      # Create entry in doc_dict if needed, else just update entry
      if doc_dict.has_key(doc_id):
	 doc_dict[doc_id] = doc_dict[doc_id] + " " + word_entry
      else: doc_dict[doc_id] = word_entry
   
   infile.close()
   
   return doc_dict


# Function to write document summary file
def doc_sum_out(outfile_name,length_dict,doc_topic_dict):
   # Ready file for writing
   outfile = open(outfile_name,"w")
   
   for entry in length_dict:
      doc_id = entry
      length = length_dict[entry]
      topic_list = doc_topic_dict[entry]
      out_string = "%s\t%s\t%s\n" % (doc_id, length, topic_list)
      outfile.write(out_string)
   
   outfile.close()


# Function to write lda data format file
def lda_dict_out(infile_ttc_name,outfile_name,doc_dict,length_dict,doc_topic_dict):
   
   # Ready files for reading and writing
   infile_ttc = open(infile_ttc_name,"r")
   outfile = open(outfile_name,"w")
   
   # Read in list of terminal topics
   term_topic_dict = {}
   for line in infile_ttc:
      # Separate input string by tabs
      line_list = string.split(line.strip(),"\t")
      doc_id = line_list[0]
      term_topic_str = line_list[2]
      term_topic_dict[doc_id] = term_topic_str
   
   infile_ttc.close()
   
   # Loop through dictionary
   for entry in doc_dict:
      
      # Unpack dictionary entry
      doc_id = entry
      word_entry = doc_dict.get(entry,"????")
      doc_length = length_dict.get(entry,"????")
      topic_str = term_topic_dict.get(str(doc_id),"????")
      
      # Setup output strings
      prefix = "%s\t%s\t%s\t%s" % (doc_id, topic_str, doc_length, word_entry)
      suffix = "\n"
      out_string = prefix + suffix
      outfile.write(out_string)
   
   outfile.close()


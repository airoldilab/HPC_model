# Script to take original LDA format file and get rid of document with no active topics and to 
# get rid of the "C1511" topic

import string

def clean_lda_data(infilename_lda,outfilename):
   
   # Load in original LDA file and do desired processing. Then spit out corrected lines to 
   # output file
   infile_lda = open(infilename_lda,"r")
   outfile = open(outfilename,"w")
   doc_topic_dict = {}
   for line in infile_lda:
      doc_id, topic_str, doc_length, word_count_str = string.split(line.strip(),"\t")
      # Skip over documents that don't have any assigned topic
      if topic_str == "None" or topic_str=="": continue
      else: 
	 # Otherwise continue processing
	 # Look for "C1511" topic in list and remove if there
	 topic_list = string.split(topic_str," ")
	 if "C1511" in topic_list: 
	    topic_list.remove("C1511")
	    # Make sure parent is there
	    if not "C151" in topic_list:
	       topic_list.append("C151")
	 topic_str = string.join(topic_list," ")
	 # Output results to outfile
	 outstring = "%s\t%s\t%s\t%s\n" % (doc_id, topic_str, doc_length, word_count_str)
	 outfile.write(outstring)
   outfile.close()
   infile_lda.close()

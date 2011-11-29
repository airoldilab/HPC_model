import sys, os
sys.path.append("/n/home13/jbischof/reuters_prj/extract_reuters_data/extract_functions/")
import getcounts
from collections import defaultdict
import string
import re

data_dir = sys.argv[1]
outfilename = sys.argv[2]

# Function to get topics for each article. Outputs file with the number of topics
# and the topics names for each doc id

def get_topic_codes(dataDir,outFilename):
   # Get list of files in dataDir
   if os.access(dataDir, os.F_OK):
      inList = os.listdir(dataDir)
      inList = [dataDir + fname for fname in inList]
   else:
      print "Error - cannot access %s" % dataDir
      sys.exit(1)
   
   # Open file for output
   outFile = open(outFilename, 'wb')
   
   for inFilename in inList:
      # Extract words from XML file
      article_xml = open(inFilename).read()
      try:
         article = getcounts.ReutersArticle(article_xml)
         if article.topic_codes != None:
	    topicCodes = [node.attrib["code"] for node in article.topic_codes.findall("code")]
	    topicCodes_str = string.join(topicCodes)
	    n_topics = len(topicCodes)
         else: 
	    topicCodes_str = None
	    n_topics = 0
	 
         # Extract article ID from filename
         match = re.search(r"[0-9]+n",inFilename)
         found_match = match.group()
         article_id = found_match[0:len(found_match)-1]
         
         docLength = len(article.tokens)
         
         # Setup output strings
         prefix = "%s\t%s\t%s" % (article_id, n_topics, topicCodes_str)
         suffix = "\n"
         outString = prefix + suffix
         
         # Write output
         outFile.write(outString)

      except: article = ""


   outFile.close()

if __name__ == "__main__":
   get_topic_codes(data_dir,outfilename)
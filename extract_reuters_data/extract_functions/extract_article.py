import sys, os
sys.path.append("/n/home13/jbischof/reuters_prj/extract_reuters_data/extract_functions/")
import getcounts
from collections import defaultdict
from nltk import PorterStemmer
import re
import string

data_dir = sys.argv[1]
outfilename = sys.argv[2]
porter_stem = sys.argv[3]

def article_extract(dataDir,outFilename,porter_stem=False):
   # Get list of files in dataDir
   if os.access(dataDir, os.F_OK):
      inList = os.listdir(dataDir)
      #inList = [fname for fname in inList if re.match(".*\.xml",fname)]
      inList = [dataDir + fname for fname in inList]
   else:
      print "Error - cannot access %s" % dataDir
      sys.exit(1)
   
   # Open file for output
   outFile = open(outFilename, 'wb')
   
   if porter_stem:
      # Setup stemmer
      porter = PorterStemmer()
   
   # Error reporting
   out_error_name = "errors_extract_article.txt"
   error_out = open(out_error_name,"w")

   for inFilename in inList:
      # Extract words from XML file
      article_xml = open(inFilename).read()
      
      # See if .xml file can be parsed; otherwise write to error file
      try: 
         article = getcounts.ReutersArticle(article_xml)
         
         # Normalize words and count unique occurences
         ngrams = [ w.lower() for w in article.tokens ]
         if porter_stem:
	    ngrams = [ porter.stem(w) for w in ngrams ]
         unique = set(ngrams)
         countList = [(u,ngrams.count(u)) for u in unique]
         
         # Get all topic codes for article (THIS NOW SEPARATE STEP---SEE "process_topic_codes.py")
         #if article.topic_codes != None:
	 #   topicCodes = [node.attrib["code"] for node in article.topic_codes.findall("code")]
	 #   topicCodes_str = string.join(topicCodes)
	 #   n_topics = len(topicCodes)
	 #else: 
	 #   topicCodes_str = None
	 #   n_topics = 0
         
         # Extract article ID from filename
         match = re.search(r"[0-9]+n",inFilename)
         found_match = match.group()
         article_id = found_match[0:len(found_match)-1]
         
         # Setup output strings
         prefix = "%s\t" % article_id
         suffix = "\n"
         outList = list()
         for word, count in countList:
	    outList.append("%s\t%d" % (word, count))
         outList = [prefix + s + suffix for s in outList]
         
         # Write output
         outFile.writelines(outList)
      
      except: error_out.write(inFilename + "\n")
      
   outFile.close()


if __name__ == "__main__":
   article_extract(data_dir,outfilename,porter_stem=False)

# Set parameters
#data_dir = "/n/scratch06/airoldi_scratch/jbischof/reuters_data"
#dir_out = "/n/airoldifs2/lab/jbischof/reuters_output/"
# Change the directories for this trial run
#data_dir = "19960820_subset/"
#dir_out = "subset_output/"
#outfile_sax = dir_out + "reuters_stemmed_art_extract.txt"
#article_extract(data_dir,outfile_sax)
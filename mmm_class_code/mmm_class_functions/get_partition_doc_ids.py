# Script to figure out which documents are in which partition
# i.e., train, valid, or test, as well as original dataset fit to LDA

import string

# Directories
main_dir = "/n/airoldifs1/jbischof/reuters_output/"
raw_data_dir = main_dir + "mmm_raw_data/"
class_dir = main_dir + "mmm_class_out/"

# Filenames
infilename_train = raw_data_dir + "reuters_train_ldaformat.txt"
infilename_valid = raw_data_dir + "reuters_valid_ldaformat.txt"
infilename_test = raw_data_dir + "reuters_test_ldaformat.txt"
outfilename_train = raw_data_dir + "reuters_train_docids.txt"
outfilename_valid = raw_data_dir + "reuters_valid_docids.txt"
outfilename_test = raw_data_dir + "reuters_test_docids.txt"

def get_partition_doc_ids(infilename_train,infilename_test, \
infilename_valid,outfilename_train,outfilename_test, \
outfilename_valid):
   
   # Train data
   infile_train_lda = open(infilename_train,"r")
   outfile_train = open(outfilename_train,"w")
   
   for line in infile_train_lda:
      doc_id, doc_topic_str, doc_length, word_str = \
      string.split(line.strip(),"\t")
      # Create doc_id file
      outstring = doc_id + "\n"
      outfile_train.write(outstring)
   
   infile_train_lda.close()
   outfile_train.close()
   
   
   # Valid data
   infile_valid_lda = open(infilename_valid,"r")
   outfile_valid = open(outfilename_valid,"w")
   
   for line in infile_valid_lda:
      doc_id, doc_topic_str, doc_length, word_str = \
      string.split(line.strip(),"\t")
      # Create doc_id file
      outstring = doc_id + "\n"
      outfile_valid.write(outstring)
   
   infile_valid_lda.close()
   outfile_valid.close()
   
   # Test data
   infile_test_lda = open(infilename_test,"r")
   outfile_test = open(outfilename_test,"w")
   
   for line in infile_test_lda:
      doc_id, doc_topic_str, doc_length, word_str = \
      string.split(line.strip(),"\t")
      # Create doc_id file
      outstring = doc_id + "\n"
      outfile_test.write(outstring)
   
   infile_test_lda.close()
   outfile_test.close()


if __name__ == "__main__":
   get_partition_doc_ids(infilename_train,infilename_test, \
   infilename_valid,outfilename_train,outfilename_test, \
   outfilename_valid)
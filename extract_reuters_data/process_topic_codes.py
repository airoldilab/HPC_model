# Script to extract all topic codes from corpus
# (instead of grabbing first code only)

import string
from get_topic_codes import *

data_dir = "/n/scratch06/airoldi_scratch/jbischof/reuters_data/"
dir_out = "/n/airoldifs1/jbischof/reuters_output/"

# Process for extracting full set of topic codes
# to assess mixed membership behavior
outfile_tc = dir_out + "reuters_topic_codes.txt"
get_topic_codes(data_dir,outfile_tc)

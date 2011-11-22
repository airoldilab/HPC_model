# Script to coordinate getting topic label data for documents in Reuters corpus

import string
import sys
import subprocess

fun_fold = "/n/home13/jbischof/reuters_prj/mmm_class_code/mmm_class_functions/"
sys.path.append(fun_fold)
from get_reuters_doc_topic_membs import *

main_dir = sys.argv[1]

# Filenames
#main_dir = "/n/airoldifs1/jbischof/reuters_output/"
raw_data_dir = main_dir + "mmm_raw_data/"
class_dir = main_dir + "mmm_class_out/"
infilename_tab = "/n/home13/jbischof/reuters_prj/mmm_class_code/reuters_topic_address_book_py.txt"


# Step 1: get plain text file with topic memberships

# Data by partition
part_list = ["train","valid","test"]
part_file_dict = {}
outfile_dict = {}
for part in part_list:
   infilename = raw_data_dir + "reuters_" + part + "_ldaformat.txt"
   outfilename = raw_data_dir + "reuters_" + part + "_topic_membs.txt"
   part_file_dict[part] = infilename
   outfile_dict[part] = outfilename

get_lda_data(part_file_dict,infilename_tab,part_list,outfile_dict)


# Step 2: get RData version of topic memberships in sparse matrix format
script2 = "get_sparse_topic_membs.R"
cmd2 = "Rscript" + " " + fun_fold + script2 + " " + main_dir
proc = subprocess.Popen([cmd2], stdout=subprocess.PIPE, shell=True)
proc.communicate()


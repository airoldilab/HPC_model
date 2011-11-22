# Script to go through topics and train SVM model

# Import modules
import string
import sys
import subprocess

# Directories
#main_dir = "/n/airoldifs1/jbischof/reuters_output/"
#cutoff = 250
main_dir = sys.argv[1]
cutoff = sys.argv[2]
model = sys.argv[3]

# Need dictionary of model argument to liblinear function
model_dict = {"logit":"0","svm":"2"}
model_number = model_dict[model]
# Probability estimates?
prob_opt_dict = {"logit":"1","svm":"0"}
prob_opt_number = prob_opt_dict[model]

raw_data_dir = main_dir + "mmm_raw_data/"
class_dir = main_dir + "mmm_class_out/"
topic_class_dir = class_dir + "topic_class_out/"
topic_probs_dir = topic_class_dir + "topic_probs/"
data_dir = raw_data_dir + "liblinear_data/"
out_dir = class_dir + model + "_liblinear_fits" + str(cutoff) + "/"


# Grab list of topics from topic address book
infilename_tab = "reuters_topic_address_book_py.txt"
topic_list = []
infile_tab = open(infilename_tab,"r")
for line in infile_tab:
   topic, level, parent = string.split(line.strip(),"\t")
   topic_list.append(topic)

#topic_list = topic_list[0:2]

for topic in topic_list:
   train_file = data_dir + "liblinear_data_train_" + topic + ".txt"
   model_file = out_dir + "train_" + topic + ".model"
   valid_file = data_dir + "liblinear_data_valid_" + topic + ".txt"
   test_file = data_dir + "liblinear_data_test_" + topic + ".txt"
   valid_output_file = out_dir + "valid_" + topic + ".out"
   test_output_file = out_dir + "test_" + topic + ".out"
   
   cmd_train = "liblinear-train -s " + model_number + " " + train_file + " " + model_file
   cmd_predict_valid = "liblinear-predict -b " + prob_opt_number + " " + valid_file + " " \
   + model_file + " " + valid_output_file
   cmd_predict_test = "liblinear-predict -b " + prob_opt_number + " " + test_file + " "  \
   + model_file + " " + test_output_file
   
   proc = subprocess.Popen([cmd_train], stdout=subprocess.PIPE, shell=True)
   proc.communicate()
   
   proc = subprocess.Popen([cmd_predict_valid], stdout=subprocess.PIPE, shell=True)
   proc.communicate()
   
   proc = subprocess.Popen([cmd_predict_test], stdout=subprocess.PIPE, shell=True)
   proc.communicate()
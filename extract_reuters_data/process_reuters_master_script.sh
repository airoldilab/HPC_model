# Master script for parsing Reuters corpus into LDA format and then 
# generating hierarachy for each word

# Step 1
# Extract list of counts for each unique unigram in each article and create 
# file reuters_stemmed_art_extract.txt
# This step actually done using LSF for full corpus
python extract_article_stemmed.py

# Step 2
# Extract list of topics for each article in corpus
# Steps 1 and 2 separated since need more than one day or processing time to do both 
# tasks at once
# This step done with LSF for full corpus as well
python process_topic_codes.py

# Step 3
# Create LDA format data, dictionary of word counts, and document summary dictionary
# Again, this done via LSF, but only takes about 30 mins or so
python process_get_lda_data.py
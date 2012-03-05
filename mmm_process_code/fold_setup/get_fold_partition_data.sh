#!/bin/bash

# Send off LSF scripts for each partition
bsub < process_valid_lda_data.lsf
bsub < process_test_lda_data.lsf
bsub < process_train_lda_data.lsf

exit 0
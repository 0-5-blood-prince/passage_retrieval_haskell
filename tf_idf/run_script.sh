#!/bin/bash

# Define the directory path
DIR_PATH="/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/20k/passages/"

# Define the number of iterations
CHUNK_SIZE=1000

# Loop through CORES from 1 to 16
for CORES in {1..16}
do
    # Construct the command and run it with time
    echo "Running with $CORES cores..."
    time ./tf_idf_v2_par "$DIR_PATH" "$CHUNK_SIZE" +RTS -N$CORES -ls -s
done

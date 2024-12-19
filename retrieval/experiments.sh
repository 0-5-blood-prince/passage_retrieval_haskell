#!/bin/bash

# Script to automate experiments for sequential and parallel versions
# Runs using different query/passage sizes and core counts, saves results, and organizes eventlog files.

# Programs and options
SEQ_PROGRAM="./retrieve_best_passage_seq_test"  # Sequential version
PAR_PROGRAM="./retrieve_best_passage_parbasic_test" # Parallel version

GHC_COMMAND="stack --resolver lts-22.33 ghc -- -O2 -Wall -threaded -rtsopts -eventlog"

# Ensure required directories exist
RESULTS_DIR="experiment_results"
SEQ_DIR="${RESULTS_DIR}/seq"
PAR_DIR="${RESULTS_DIR}/par"

mkdir -p "$SEQ_DIR"
mkdir -p "$PAR_DIR"

# Define query and passage embedding files
# QUERY_SIZES=("0.001k" "0.01k" "0.1k")
QUERY_SIZES=("0.001k")
PASSAGE_SIZES=("200k")

# PASSAGE_SIZES=("1k" "10k" "200k")
CORE_COUNTS=(2 4 8)

QUERY_DIR="queries"
PASSAGE_DIR="passages"

# Compile the Haskell programs
echo "Compiling the sequential version..."
stack --resolver lts-22.33 ghc -- -O2 -Wall -rtsopts -eventlog retrieve_best_passage_seq_test.hs -o retrieve_best_passage_seq_test
if [ $? -ne 0 ]; then
    echo "Compilation of sequential version failed. Exiting."
    exit 1
fi

echo "Compiling the parallel version..."
$GHC_COMMAND retrieve_best_passage_parbasic_test.hs -o retrieve_best_passage_parbasic_test
if [ $? -ne 0 ]; then
    echo "Compilation of parallel version failed. Exiting."
    exit 1
fi

# Run experiments
for query_size in "${QUERY_SIZES[@]}"; do
    for passage_size in "${PASSAGE_SIZES[@]}"; do
        # Define file names
        QUERY_FILE="${QUERY_DIR}/${query_size}_query_embeddings.csv"
        PASSAGE_FILE="${PASSAGE_DIR}/${passage_size}_passage_embeddings.csv"

        # Check if files exist
        if [[ ! -f "$QUERY_FILE" || ! -f "$PASSAGE_FILE" ]]; then
            echo "Skipping experiment: Missing $QUERY_FILE or $PASSAGE_FILE"
            continue
        fi

        # Sequential version experiment
        SEQ_EXPERIMENT_NAME="query_${query_size}_passage_${passage_size}"
        SEQ_EXPERIMENT_DIR="${SEQ_DIR}/${SEQ_EXPERIMENT_NAME}"
        mkdir -p "$SEQ_EXPERIMENT_DIR"
        RTS_OPTIONS="+RTS -ls -s"
        echo "Running sequential experiment: Query=${query_size}, Passage=${passage_size}"
        time $SEQ_PROGRAM "$QUERY_FILE" "$PASSAGE_FILE" $RTS_OPTIONS > "${SEQ_EXPERIMENT_DIR}/output.txt" 2>&1
        echo "Sequential experiment completed: Results in ${SEQ_EXPERIMENT_DIR}"

        # Parallel version experiment (iterate over core counts)
        for cores in "${CORE_COUNTS[@]}"; do
            PAR_EXPERIMENT_NAME="query_${query_size}_passage_${passage_size}_cores_${cores}"
            PAR_EXPERIMENT_DIR="${PAR_DIR}/${PAR_EXPERIMENT_NAME}"
            mkdir -p "$PAR_EXPERIMENT_DIR"

            echo "Running parallel experiment: Query=${query_size}, Passage=${passage_size}, Cores=${cores}"

            RTS_OPTIONS="+RTS -N${cores} -ls -s"
            time $PAR_PROGRAM "$QUERY_FILE" "$PASSAGE_FILE" $RTS_OPTIONS > "${PAR_EXPERIMENT_DIR}/output.txt" 2>&1

            # Move eventlog file to the parallel experiment directory
            if [ -f "retrieve_best_passage_parbasic_test.eventlog" ]; then
                mv "retrieve_best_passage_parbasic_test.eventlog" "${PAR_EXPERIMENT_DIR}/eventlog.eventlog"
            fi

            echo "Parallel experiment completed: Results in ${PAR_EXPERIMENT_DIR}"
        done
    done
done

echo "All experiments completed. Results are in the $RESULTS_DIR directory."

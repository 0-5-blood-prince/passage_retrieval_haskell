To Generate TF_IDF embeddings from the Text Data:

Step 1: Please clone this repo: https://github.com/0-5-blood-prince/passage_retrieval_haskell/tree/master

Step 2: Download the dataset.

Step 3: In team_proj/passage_retrieval_haskell/tf_idf/run_script.sh update the DIR_PATH with the passages folder path in the dataset.

Step 4: Compite the program with: stack ghc -- -O2 -Wall -threaded -rtsopts -eventlog tf_idf_v2_par

Step 5: run ./run_script.sh
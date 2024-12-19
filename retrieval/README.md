To run experiments on retrieval, download the embeddings data from this link [https://drive.google.com/file/d/1aPk4ONRo_qZ7dnjmK5TJrVQUq0lDIVAr/view?usp=drive_link].

Interactive code

retrieve_best_passage_par_interactive.hs file loads the query embeddings and passage embeddings and the user can supply queryIds to get the bestPassageId interactively to run this. Compile and Run 

```
./retrieve_best_passage_par_interactive "queries/1k_query_embeddings.csv" "passages/  200k_passage_embeddings.csv" +RTS -N4 -ls -s
```

All other files are test files that run tests after loading test query_embeddings and passage_embeddings. Each file performs a computation startegy
Seqential : (retrieve_best_passage_seq_test.hs)
Parallel Cosine Similarity Strategy: (retrieve_best_passage_parcos_test.hs)
Parallel Passages Similarity Strategy(Basic): (retrieve_best_passage_parbasic_test.hs)
Parallel Passages Similarity Strategy(Chunk-Based): (retrieve_best_passage_parchunk_test.hs)

To run these files, compile them and run
```
./retrieve_best_passage_{} "{QUERY_PATH} "{PASSAGE_PATH}" +RTS -N{cores} -ls -s
```

You can make use of experiments.sh file, that runs these commands for different test query embedding files, passage embedding files and cores.

Test query files: 0.001k queries, 0.01k queries, 0.1k queries, 1k queries.
Test Passage files: 1k passages, 10k passages, 200k passages.

NOTE: Please change the chunk size manually in the code before running the chunk strategy code, Also in experiments.sh file, configure the names of sequential and parallel files before running.




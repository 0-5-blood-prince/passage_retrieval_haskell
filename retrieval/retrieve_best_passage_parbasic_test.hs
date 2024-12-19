module Main where

import System.IO (hFlush, stdout)
import Data.List (maximumBy, transpose)
import Data.Ord (comparing)
import Data.Vector (Vector)
import Control.Parallel.Strategies (parMap, rdeepseq, rpar)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import System.Environment (getArgs, getProgName)

type Embedding = Vector Double
type IdEmbedding = (Int, Embedding)


-- Parsing the CSV file into an (Id, Embedding) tuple
parseCSVLine :: T.Text -> Either [Char] IdEmbedding
parseCSVLine line = case T.splitOn (T.pack ",") line of
    [idText, embText] -> case TR.decimal idText of
        Right (id, _) -> Right (id, parseEmbeddingText embText)
        Left _ -> Left ("Invalid ID format " ++ show idText)
    _ -> Left ("Invalid line format " ++ show line)

parseEmbeddingText :: T.Text -> Embedding
parseEmbeddingText embText = V.fromList $ map (read . T.unpack) (T.splitOn (T.pack ":") embText)



readEmbeddings :: FilePath -> IO [IdEmbedding]
readEmbeddings filePath = do
    content <- TIO.readFile filePath
    let parsedLines = map parseCSVLine (T.lines content)
    case sequence parsedLines of
        Right embeddings -> return embeddings
        Left err -> error err

-- Compute the cosine similarity between two embeddings
cosineSimilarity :: Embedding -> Embedding -> Double
cosineSimilarity v1 v2 =
    let dotProduct = V.sum $ V.zipWith (*) v1 v2
        norm1 = sqrt $ V.sum $ V.map (** 2) v1
        norm2 = sqrt $ V.sum $ V.map (** 2) v2
    in dotProduct / (norm1 * norm2)

computeSimilarities :: Embedding -> [IdEmbedding] -> [(Int, Double)]
computeSimilarities queryEmbedding passages =
    let compute idEmb = ( fst idEmb , cosineSimilarity queryEmbedding (snd idEmb))
    in parMap rdeepseq compute passages

-- Find the best passage for a given query
findBestPassage :: Embedding -> [IdEmbedding] -> Int
findBestPassage queryEmbedding passages =
    let similarities = computeSimilarities queryEmbedding passages
     in fst $ maximumBy (comparing snd) similarities

printTuple :: (Int, Int) -> IO ()
printTuple (queryId, bestPassageId) = putStrLn $ "(" ++ show queryId ++ ", " ++ show bestPassageId ++ ")"

runTests :: [IdEmbedding] -> [IdEmbedding] -> IO ()
runTests testQueryEmbeddings passageEmbeddings = do
    putStrLn "Testing..."
    let results = [ (queryId, findBestPassage queryEmbedding passageEmbeddings) 
                  | (queryId, queryEmbedding) <- testQueryEmbeddings ]
    mapM_ printTuple results
    putStrLn "Done."


main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [queryEmbeddingsPath, passageEmbeddingsPath] -> do
            putStrLn "Loading test query embeddings..."
            testQueryEmbeddings <- readEmbeddings queryEmbeddingsPath
            putStrLn "Loading passage embeddings..."
            passageEmbeddings <- readEmbeddings passageEmbeddingsPath
            putStrLn "Embeddings loaded. Ready for input."
            runTests testQueryEmbeddings passageEmbeddings
        _ -> 
            putStrLn $ "Usage: " ++ progName ++ "<test_embeddings_file> <passage_embeddings_file>"

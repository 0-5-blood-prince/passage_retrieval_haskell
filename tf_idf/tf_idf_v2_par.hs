import Data.Word
import Data.Char (toLower)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO ( hClose, hPutStrLn, openFile, IOMode(WriteMode), hGetContents, withFile, IOMode(ReadMode), hGetContents )
import Data.List.Split (splitOn)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.DeepSeq (deepseq)
import Control.Parallel.Strategies (using, parList, rdeepseq)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Time (diffUTCTime)
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)


createWordList :: FilePath -> IO [String]
createWordList filePath = 
    withFile filePath ReadMode $ \handle -> do
        content <- hGetContents handle
        content `deepseq` return ()
        return $ lines content

-- Type alias for better readability
type PassageMap = Map.Map String String

-- Function to read all passages from files in a directory
readPassagesFromDirectory :: FilePath -> IO PassageMap
readPassagesFromDirectory dir = do
    fileNames <- listDirectory dir    
    let fileNameFullPath = map (\file_name -> dir ++ file_name) fileNames
    passageMaps <- mapM readPassagesFromFile fileNameFullPath
    return $ Map.unions passageMaps

readPassagesFromFile :: FilePath -> IO PassageMap
readPassagesFromFile fileName = 
    withFile fileName ReadMode $ \handle -> do
        content <- hGetContents handle
        content `deepseq` return ()
        let linesOfFile = lines content
        let keyValuePairs = map parseLine linesOfFile
        return $ Map.fromList keyValuePairs

parseLine :: String -> (String, String)
parseLine line =
    let parts = splitOn "," line
    in case parts of
        (key:rest) -> (key, unwords rest)  -- Combine the rest into the passage
        _ -> error $ "Invalid line format: " ++ line

tfIdf :: [String] -> [String] -> Map.Map String Double -> Map.Map String Double
tfIdf wordsList passageWords idf =
  let totalWords = fromIntegral (length passageWords)
      wordCounts = Map.fromListWith (+) [(word, 1) | word <- passageWords]
  in Map.fromList [(word, (fromMaybe 0 (Map.lookup word wordCounts)) * (fromMaybe 0 (Map.lookup word idf)) / totalWords) | word <- wordsList]

-- Convert a map of TF values to a CSV row format
mapToCsvRow :: [String] -> Map.Map String Double -> String
mapToCsvRow wordsList tfMap =
  let tfValues = [show (fromMaybe 0 (Map.lookup word tfMap)) | word <- wordsList]
  in intercalate "," tfValues

processAndWriteCsv :: [String] -> [(String, String)] -> Map.Map String Double -> Map.Map String String
processAndWriteCsv wordsList passages idf =
    let passageMap = Map.fromList passages
    in Map.map (\passage -> 
        let passageWords = words (map toLower passage)
            tfValues = tfIdf wordsList passageWords idf
        in mapToCsvRow wordsList tfValues) passageMap


readMapFromCsv :: FilePath -> IO (Map.Map String Double)
readMapFromCsv filePath = do
  csvData <- BL.readFile filePath
  case Csv.decode Csv.HasHeader csvData of
    Left err -> error err
    Right vec -> return $ Map.fromList [(key, value) | (key, value) <- V.toList vec]

saveMapToCSV :: FilePath -> Map.Map String String -> [String] -> IO ()
saveMapToCSV path mapData wordList = do
    let csvData = Map.foldrWithKey (\key value acc -> [key, value] : acc) [] mapData
        headers = ["passageId", intercalate "," wordList] -- Headers for CSV
    BL.writeFile path $ Csv.encode (headers : csvData) -- Write to file

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (ys, zs) = splitAt n xs in ys : chunk n zs

-- IDF LOGIC #############################
createWordMap :: FilePath -> IO (Map.Map String Double)
createWordMap filePath = 
    withFile filePath ReadMode $ \handle -> do
        content <- hGetContents handle
        content `deepseq` return ()
        let wordsInFile = lines content
        let wordMap = Map.fromList [(word, 0) | word <- wordsInFile]
        -- Return the resulting Map
        return wordMap

idf :: [String] -> (Map.Map String Double) -> (Map.Map String Double)
idf passages initMap = 
    let passageSets = map sentenceToSet passages
        idf_count = foldr addSetToMap initMap passageSets 
    in idf_count

sentenceToSet sentence = Set.fromList $ map (map toLower) (words sentence)

addSetToMap passage_set doc_count = foldr (\word acc' -> Map.adjust (1.0 +) word acc') doc_count (Set.elems passage_set)

addDocToMap docMap count = 
    let passages = Map.elems docMap
        passageSets = map sentenceToSet passages
    in foldr addSetToMap count passageSets

idfNormalise x totalDocCount = logBase 2 ((fromIntegral totalDocCount) / x) 
-- #######################################

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [dir_path, chunkSizeString] -> do
            let filePath = "output.txt"
            let chunkSize = read chunkSizeString
            wordOrder <- createWordList filePath
            word_map <- createWordMap filePath
            all_passages <- readPassagesFromDirectory dir_path

            -- IDF logic
            let passage_count = Map.size all_passages
            let passage_chunks = chunk chunkSize (Map.elems all_passages)
            let par_output_idf = map (\input -> idf input word_map) passage_chunks `using` parList rdeepseq
            let reduced_output = Map.unionsWith (+) par_output_idf
            let norm_idf = Map.map (\x -> idfNormalise x passage_count) reduced_output

            norm_idf `deepseq` return ()

            --TF logic
            let par_input = chunk chunkSize (Map.toList all_passages)
            let par_output = map (\t_p_input -> processAndWriteCsv wordOrder t_p_input norm_idf) par_input `using` parList rdeepseq
            let output = Map.unions par_output
            -- let output = processAndWriteCsv wordOrder (Map.toList all_passages) idf
            saveMapToCSV "tf_idf_par_output.csv" output wordOrder

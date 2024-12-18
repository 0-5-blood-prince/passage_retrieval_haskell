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
import Data.Csv (encode, record)
import qualified Data.ByteString.Lazy as B


createWordMap :: FilePath -> IO (Map.Map String Double)
createWordMap filePath = 
    withFile filePath ReadMode $ \handle -> do
        content <- hGetContents handle
        content `deepseq` return ()
        let wordsInFile = lines content
        let wordMap = Map.fromList [(word, 0) | word <- wordsInFile]
        -- Return the resulting Map
        return wordMap

-- Type alias for better readability
type PassageMap = Map.Map String String

-- Function to read all passages from files in a directory
readPassagesFromDirectory :: FilePath -> IO PassageMap
readPassagesFromDirectory dir = do
    fileNames <- listDirectory dir    
    let totalFiles = length fileNames
    passageMaps <- mapM (processFileWithProgress totalFiles) (zip [1..] fileNames)
    return $ Map.unions passageMaps

-- Function to process a file with progress tracking
processFileWithProgress :: Int -> (Int, FilePath) -> IO PassageMap
processFileWithProgress totalFiles (index, fileName) = do
    -- Process the file and get the result
    let dir_path = "/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/20k_ver_1/passages/"

    passageMap <- readPassagesFromFile (dir_path ++ fileName)
    
    -- Print progress (index out of total files)
    putStrLn $ "Processed " ++ show index ++ " of " ++ show totalFiles ++ " files."
    
    -- Return the passage map
    return passageMap

readPassagesFromFile :: FilePath -> IO PassageMap
readPassagesFromFile fileName = 
    withFile fileName ReadMode $ \handle -> do
        -- Read the file contents
        content <- hGetContents handle
        -- Force evaluation of the contents
        content `deepseq` return ()
        
        -- Split the content into lines
        let linesOfFile = lines content
        
        -- Parse each line into a key-value pair (passageId, passage)
        let keyValuePairs = map parseLine linesOfFile
        
        -- Create a Map from the list of key-value pairs
        return $ Map.fromList keyValuePairs

parseLine :: String -> (String, String)
parseLine line =
    let parts = splitOn "," line
    in case parts of
        (key:rest) -> (key, unwords rest)  -- Combine the rest into the passage
        _ -> error $ "Invalid line format: " ++ line

-- Save the map to a CSV file
saveMapToCSV :: FilePath -> Map.Map String Double -> IO ()
saveMapToCSV path mapData = do
    let csvData = Map.foldrWithKey (\key value acc -> [key, show value] : acc) [] mapData
        headers = ["Key", "Value"] -- Headers for CSV
    B.writeFile path $ encode (headers : csvData) -- Write to file

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

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (ys, zs) = splitAt n xs in ys : chunk n zs

chunkSize = 1000

main :: IO ()
main = do
    let filePath = "output.txt"
    word_map <- createWordMap filePath
    let dir_path = "/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/20k_ver_1/passages/"
    all_passages <- readPassagesFromDirectory dir_path
    let passage_count = Map.size all_passages
    print passage_count
    let passage_chunks = chunk chunkSize (Map.elems all_passages)
    let par_output = map (\input -> idf input word_map) passage_chunks `using` parList rdeepseq
    let reduced_output = Map.unionsWith (+) par_output
    -- let reduced_output = idf (Map.elems all_passages) word_map
    saveMapToCSV "idf_output_par.csv" (Map.map (\x -> idfNormalise x passage_count) reduced_output)

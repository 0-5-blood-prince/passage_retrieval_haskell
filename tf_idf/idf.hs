import qualified Data.Map as Map
import System.IO (withFile, IOMode(ReadMode), hGetContents)
import Control.DeepSeq (deepseq)
import Data.Char (toLower)
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import System.Directory (listDirectory)
import Data.Csv (encode, record)
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.Async (mapConcurrently)




createWordMap :: FilePath -> IO (Map.Map String Double)
createWordMap filePath = 
    withFile filePath ReadMode $ \handle -> do
        content <- hGetContents handle
        content `deepseq` return ()
        let wordsInFile = lines content
        let wordMap = Map.fromList [(word, 0) | word <- wordsInFile]
        -- Return the resulting Map
        return wordMap


chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (ys, zs) = splitAt n xs in ys : chunk n zs

chunkSize = 100

-- Function to read all passages from files in a directory
readPassagesFromDirectory :: FilePath -> (Map.Map String Double) -> IO ((Map.Map String Double), Int)
readPassagesFromDirectory dir count = do
    fileNames <- listDirectory dir
    let totalFiles = length fileNames

    let cnt = 0
    processFiles totalFiles fileNames count cnt

processFiles :: Int -> [FilePath] -> (Map.Map String Double) -> Int -> IO ((Map.Map String Double), Int)
processFiles _ [] count cnt = return (count, cnt) 
processFiles totalFiles (fileName:rest) count cnt = do
    (count_1, cnt_1)<- processFileWithProgress totalFiles fileName count cnt
    processFiles totalFiles rest count_1 cnt_1

-- Function to process a file with progress tracking
processFileWithProgress :: Int -> FilePath -> (Map.Map String Double) -> Int -> IO ((Map.Map String Double), Int)
processFileWithProgress totalFiles fileName count cnt = do
    -- Process the file and get the result
    let dir_path = "/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/20k_ver_1/passages/"
    (count_1, cnt_1) <- readPassagesFromFile (dir_path ++ fileName) count cnt
    return (count_1, cnt_1)

readPassagesFromFile :: FilePath -> (Map.Map String Double) -> Int -> IO ((Map.Map String Double), Int)
readPassagesFromFile fileName count cnt = 
    withFile fileName ReadMode $ \handle -> do
        content <- hGetContents handle
        content `deepseq` return ()
        let linesOfFile = lines content
        let keyValuePairs = map parseLine linesOfFile
        let docMap = Map.fromList keyValuePairs
        return (addDocToMap docMap count, cnt + (Map.size docMap))

sentenceToSet sentence = Set.fromList $ map (map toLower) (words sentence)

addSetToMap passage_set doc_count = foldr (\word acc' -> Map.adjust (1.0 +) word acc') doc_count (Set.elems passage_set)

addDocToMap docMap count = 
    let passages = Map.elems docMap
        passageSets = map sentenceToSet passages
    in foldr addSetToMap count passageSets

idfNormalise x totalDocCount = logBase 2 ((fromIntegral totalDocCount) / x) 

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

main :: IO ()
main = do
    let filePath = "output.txt"
    let dir_path = "/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/20k_ver_1/passages/"
    word_map <- createWordMap filePath
    (word_doc_count, passage_cnt) <- readPassagesFromDirectory dir_path word_map
    print passage_cnt

    saveMapToCSV "idf_output.csv" (Map.map (\x -> idfNormalise x passage_cnt) word_doc_count)
    -- saveMapToFile "idf_output.txt" word_doc_count
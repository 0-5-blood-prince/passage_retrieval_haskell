import qualified Data.Map as Map
import System.IO (withFile, IOMode(ReadMode), hGetContents, appendFile)
import Data.List (words, nub)
import Control.DeepSeq (deepseq)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import Data.List (tail)
import qualified Data.Vector as V
import Text.XHtml (header)
import System.Directory (listDirectory)




type PassageMap = Map.Map String String

createWordList :: FilePath -> IO [String]
createWordList filePath = 
    withFile filePath ReadMode $ \handle -> do
        content <- hGetContents handle
        content `deepseq` return ()
        return $ lines content

-- Function to read all passages from files in a directory
readPassagesFromDirectory :: FilePath -> (Map.Map String Float) -> [String] -> IO ()
readPassagesFromDirectory dir idf wordList = do
    fileNames <- listDirectory dir
    let header = "passageId," ++ intercalate "," wordList
    writeFile "tf_idf_output.csv" header
    let totalFiles = length fileNames
    processFiles totalFiles (zip [1..] fileNames) idf wordList

processFiles :: Int -> [(Int, FilePath)] -> (Map.Map String Float) -> [String] -> IO ()
processFiles _ [] idf wordList = return ()
processFiles totalFiles ((index, fileName):rest) idf wordList = do
    -- Process the current file
    processFileWithProgress totalFiles (index, fileName) idf wordList
    processFiles totalFiles rest idf wordList

-- Function to process a file with progress tracking
processFileWithProgress :: Int -> (Int, FilePath) -> (Map.Map String Float) -> [String] -> IO ()
processFileWithProgress totalFiles (index, fileName) idf wordList = do
    let dir_path = "/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/20k_ver_1/passages/"

    readPassagesFromFile (dir_path ++ fileName) idf wordList
    -- Print progress (index out of total files)
    putStrLn $ "Processed " ++ show index ++ " of " ++ show totalFiles ++ " files."
    
    return ()

readPassagesFromFile :: FilePath -> (Map.Map String Float) -> [String] -> IO ()
readPassagesFromFile fileName idf wordList = 
    withFile fileName ReadMode $ \handle -> do
        -- Read the file contents
        content <- hGetContents handle
        -- Force evaluation of the contents
        content `deepseq` return ()
        -- Split the content into lines
        let linesOfFile = lines content
        -- Parse each line into a key-value pair (passageId, passage)
        let keyValuePairs = map parseLine (tail linesOfFile)
        processAndWriteCsv wordList keyValuePairs idf "tf_idf_output.csv"

parseLine :: String -> (String, String)
parseLine line =
    let parts = splitOn "," line
    in case parts of
        (key:rest) -> (key, unwords rest)  -- Combine the rest into the passage
        _ -> error $ "Invalid line format: " ++ line


tfIdf :: [String] -> [String] -> Map.Map String Float -> Map.Map String Float
tfIdf wordsList passageWords idf =
  let totalWords = fromIntegral (length passageWords)
      wordCounts = Map.fromListWith (+) [(word, 1) | word <- passageWords]
  in Map.fromList [(word, (fromMaybe 0 (Map.lookup word wordCounts)) * (fromMaybe 0 (Map.lookup word idf)) / totalWords) | word <- wordsList]

-- Convert a map of TF values to a CSV row format
mapToCsvRow :: String -> [String] -> Map.Map String Float -> String
mapToCsvRow passageId wordsList tfMap =
  let tfValues = [show (fromMaybe 0 (Map.lookup word tfMap)) | word <- wordsList]
  in intercalate "," (passageId : tfValues)

processAndWriteCsv :: [String] -> [(String, String)] -> Map.Map String Float -> String -> IO ()
processAndWriteCsv wordsList passages idf outputFile = do
  let rows = map (\(pid, passage) -> 
                   let passageWords = words (map toLower passage)
                       tfValues = tfIdf wordsList passageWords idf
                   in mapToCsvRow pid wordsList tfValues) passages
  appendFile outputFile (unlines rows)


readMapFromCsv :: FilePath -> IO (Map.Map String Float)
readMapFromCsv filePath = do
  csvData <- BL.readFile filePath
  case Csv.decode Csv.HasHeader csvData of
    Left err -> error err
    Right vec -> return $ Map.fromList [(key, value) | (key, value) <- V.toList vec]



main :: IO ()
main = do
    wordOrder <- createWordList "output.txt"
    -- processAndWriteCsv wordOrder passages "tf_idf_output.csv"
    idf <- readMapFromCsv "idf_output.csv"
    readPassagesFromDirectory "/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/20k_ver_1/passages/" idf wordOrder
    -- clearFile "tf_idf_output.txt"
    -- appendTFsToFile "tf_idf_output.txt" passageMap wordOrder
    -- print idf


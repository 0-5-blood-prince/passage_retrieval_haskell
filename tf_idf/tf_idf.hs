import qualified Data.Map as Map
import Data.Word
import Data.Maybe (fromMaybe)
import System.IO ( hClose, hPutStrLn, openFile, IOMode(WriteMode), hGetContents, withFile, IOMode(ReadMode), hGetContents )
import Data.List.Split (splitOn)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Control.DeepSeq (deepseq)


-- Type alias for better readability
type PassageMap = Map.Map String String

-- Function to read all passages from files in a directory
readPassagesFromDirectory :: FilePath -> IO PassageMap
readPassagesFromDirectory dir = do
    -- Get all file names in the directory
    fileNames <- listDirectory dir
    
    -- Total number of files for progress tracking
    let totalFiles = length fileNames

    -- Read and parse each file, printing progress
    passageMaps <- mapM (processFileWithProgress totalFiles) (zip [1..] fileNames)
    
    -- Combine all maps into a single map
    return $ Map.unions passageMaps

-- Function to process a file with progress tracking
processFileWithProgress :: Int -> (Int, FilePath) -> IO PassageMap
processFileWithProgress totalFiles (index, fileName) = do
    -- Process the file and get the result
    let dir_path = "/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/20k/passages/"

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


normaliseTermFrequencyAndMultIdf :: Map.Map String Int -> Map.Map String Double -> Map.Map String Double
normaliseTermFrequencyAndMultIdf wordsMap idfMap = 
    Map.mapWithKey (\word count -> (fromIntegral count / totalWords) * Map.findWithDefault 1.0 word idfMap) wordsMap
  where totalWords = fromIntegral $ sum (Map.elems wordsMap)

termFrequency :: Num a => [Char] -> Map.Map String a
termFrequency doc = agg wrds
    where 
        wrds = words doc
        agg [] = Map.empty
        agg (x:xs) = Map.insertWith (+) x 1 (agg xs) 

mapTermFreq :: Num a => Map.Map String String -> Map.Map String (Map.Map String a)
mapTermFreq docMap = Map.map termFrequency docMap

inverseDocumentFrequency :: Map.Map String (Map.Map String Int) -> Map.Map String Double
inverseDocumentFrequency docWordCnt = Map.map idf wordDocCnts
    where 
        totalDocs = fromIntegral $ Map.size docWordCnt
        countDocsAgg wordCnt acc = foldr (\word acc' -> Map.insertWith (+) word 1.0 acc') acc (Map.keys wordCnt)
        wordDocCnts = foldr countDocsAgg Map.empty (Map.elems docWordCnt)
        idf docCnt = logBase 2 (totalDocs / docCnt)

tfIdf :: Map.Map String String -> Map.Map String (Map.Map String Double)
tfIdf inputMap = Map.map temp_f tf
    where
        tf = Map.map termFrequency inputMap
        idf = inverseDocumentFrequency tf
        temp_f inp = normaliseTermFrequencyAndMultIdf inp idf

readWordsFromFile :: FilePath -> IO [String]
readWordsFromFile fileName = do
    content <- readFile fileName
    return (words content)

convertMap :: Map.Map String (Map.Map String Double) -> [String] -> Map.Map String [Double]
convertMap outerMap keys = Map.map (\innerMap -> map (\key -> fromMaybe 0 (Map.lookup key innerMap)) keys) outerMap

formatResult :: Map.Map String [Double] -> String
formatResult = unlines . map formatEntry . Map.toList
  where
    formatEntry (k, vs) = k ++ "," ++ formatList vs

formatList :: [Double] -> String
formatList = concatMap showComma
  where
    showComma x = show x ++ ":"
    removeTrailingComma xs = if not (null xs) then init xs else xs

main :: IO ()
main = do
    -- let testing_map = Map.fromList [("1", "good boy"), ("2", "good girl"), ("3", "boy girl good")]
    let dir_path = "/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/20k/passages/"
    all_passages <- readPassagesFromDirectory dir_path
    let res = tfIdf all_passages
    wordList <- readWordsFromFile "output.txt"
    let con_map = convertMap res wordList
    -- print tf_val
    -- print wordList
    let fileName = "tf_idf_vals.txt"
    writeFile fileName $ formatResult con_map
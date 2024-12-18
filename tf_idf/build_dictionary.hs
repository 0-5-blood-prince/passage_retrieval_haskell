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



-- Type alias for better readability
type PassageMap = Map.Map String String

-- Function to read all passages from files in a directory
readPassagesFromDirectory :: FilePath -> IO PassageMap
readPassagesFromDirectory dir = do
    fileNames <- listDirectory dir    
    let totalFiles = length fileNames
    passageMaps <- mapConcurrently (processFileWithProgress totalFiles) (zip [1..] fileNames)
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


saveSetToFile :: FilePath -> Set.Set String -> IO ()
saveSetToFile filePath set = do
    handle <- openFile filePath WriteMode
    mapM_ (hPutStrLn handle) (Set.toList set)
    hClose handle

addSentenceToSet :: Set.Set String -> String -> Set.Set String
addSentenceToSet set sentence = 
    let wrds = map (map toLower) (words sentence) 
    in Set.union (Set.fromList wrds) set

buildSet :: [String] -> Set.Set String
buildSet [] = Set.empty
buildSet (x:xs) = addSentenceToSet (buildSet xs) x

addSetToMap set acc = foldr (\word acc' -> Map.insertWith (+) word 1.0 acc') acc (Set.elems set)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (ys, zs) = splitAt n xs in ys : chunk n zs

chunkSize = 100

main :: IO ()
main = do
    let dir_path = "/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/20k_ver_1/passages/"
    all_passages <- readPassagesFromDirectory dir_path
    print $ Map.size all_passages
    let test_input = Map.elems all_passages
    -- start <- getCurrentTime
    let par_input = chunk chunkSize test_input
    print $ length par_input
    let par_output = map buildSet par_input `using` parList rdeepseq
    let test_set = Set.unions par_output
    let filePath = "output.txt"
    saveSetToFile filePath test_set
    -- end <- getCurrentTime
    -- let diff = diffUTCTime end start 
    -- print diff
    -- print "Program ended"


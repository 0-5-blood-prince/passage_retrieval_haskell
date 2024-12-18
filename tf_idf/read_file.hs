import qualified Data.Map as Map
import Data.List.Split (splitOn)

readPassagesFromFile :: FilePath -> IO (Map.Map String String)
readPassagesFromFile fileName = do
    content <- readFile fileName
    let linesOfFile = lines content
    let keyValuePairs = map parseLine linesOfFile
    return $ Map.fromList keyValuePairs

parseLine :: String -> (String, String)
parseLine line =
    let parts = splitOn "," line
    in case parts of
        (key:rest) -> (key, unwords rest)  -- Combine the rest into the passage
        _ -> error $ "Invalid line format: " ++ line

main :: IO ()
main = do
    let path = "/Users/samhitbhogavalli/Documents/parallel_fuctional_programming/team_proj/output/passages/0_passages.csv"
    res <- readPassagesFromFile path
    print $ Map.lookup "0" res
import Data.Word
import Data.Char (toLower)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO ( hClose, hPutStrLn, openFile, IOMode(WriteMode) )


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

main :: IO ()
main = do
    let test_map = Map.fromList [("1", "good boy"), ("2", "good girl"), ("3", "boy girl good")]
    let input = "I am testing testing"
    let test_input = Map.elems test_map
    let test_set = buildSet test_input
    let filePath = "output.txt"
    saveSetToFile filePath test_set


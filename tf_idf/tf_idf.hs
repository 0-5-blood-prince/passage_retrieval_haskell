import qualified Data.Map as Map
import Data.Word
import Data.Maybe (fromMaybe)

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
        idf docCnt = logBase 2 (totalDocs / (docCnt))

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
    -- Format each map entry as "key: value1,value2,value3"
    formatEntry (k, vs) = k ++ "," ++ formatList vs

formatList :: [Double] -> String
formatList = concatMap showComma
  where
    -- Add commas between elements but not at the end
    showComma x = show x ++ ":"
    removeTrailingComma xs = if not (null xs) then init xs else xs
    -- Call removeTrailingComma to ensure no trailing comma
    -- Final step removing, clean

main :: IO ()
main = do
    let testing_map = Map.fromList [("1", "good boy"), ("2", "good girl"), ("3", "boy girl good")]
    let res = tfIdf testing_map
    let tf_val = inverseDocumentFrequency $ Map.map termFrequency testing_map
    wordList <- readWordsFromFile "output.txt"
    let con_map = convertMap res wordList
    -- print tf_val
    -- print wordList
    let fileName = "tf_idf_vals.txt"
    writeFile fileName $ formatResult con_map
import qualified Data.Map as Map
import Data.Word

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
        idf docCnt = logBase 2 (totalDocs / (1.0 + docCnt))

tfIdf inputMap = Map.map temp_f tf
    where
        tf = Map.map termFrequency inputMap
        idf = inverseDocumentFrequency tf
        temp_f inp = normaliseTermFrequencyAndMultIdf inp idf



main :: IO ()
main = do
    print "hello"
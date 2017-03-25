module VocabLoader where

import StringUtils (splitAndStrip)
import Data.List.Split (splitOn)


type VocabMapping = (Int, Int, String, String)
type Vocab = [VocabMapping]


{-|
  Converts innermost String lists to (String, String).
-}
unsafeAsTuples :: [[[String]]] -> [[(String, String)]]
unsafeAsTuples = map (map (\x -> (x !! 0, x !! 1)))


flattenedVocab :: [[(String, String)]] -> Vocab
flattenedVocab vocab = chapterIter 0 0 vocab

chapterIter :: Int -> Int -> [[(String, String)]] -> Vocab
chapterIter _ _ [] = []
chapterIter chapterNum mappingId (chapter:xs) =
  (mappingIter chapterNum mappingId chapter) ++
  chapterIter (chapterNum + 1) (mappingId + length chapter) xs

mappingIter :: Int -> Int -> [(String, String)] -> Vocab
mappingIter _ _ [] = []
mappingIter chapterNum mappingId ((german, english):xs) =
  (chapterNum, mappingId, german, english) : mappingIter chapterNum (mappingId + 1) xs


loadVocab :: IO Vocab
loadVocab = do
  putStr "Loading words...\n"
  contents <- readFile "words.txt"
  -- Extract English -> German word mappings from raw input.
  -- Split each line on "=>", then strip the leading/trailing whitespace on
  -- every string, and then split on empty lines to separate each list of
  -- mappings into their respective chapters.
  return (flattenedVocab . unsafeAsTuples . splitOn [[]] . map (splitAndStrip "=>") . lines $ contents)

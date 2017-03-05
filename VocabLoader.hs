module VocabLoader where

import StringUtils (splitAndStrip)
import Data.List.Split (splitOn)

loadVocab :: IO ([[[String]]])
loadVocab = do
  putStr "Loading words...\n"
  contents <- readFile "words.txt"
  -- Extract English -> German word mappings from raw input.
  -- Split each line on "=>", then strip the leading/trailing whitespace on
  -- every string, and then split on empty lines to separate each list of
  -- mappings into their respective chapters.
  return (splitOn [[]] . map (splitAndStrip "=>") . lines $ contents)

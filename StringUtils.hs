module StringUtils where

import Data.String.Utils (strip)

contains :: [String] -> String -> Bool
contains list str
  | null list = False
  | (head list) == str = True
  | otherwise = contains (tail list) str


split :: String -> String -> [String]
split splitStr str = aux str "" []
  where aux str' section acc
          | str' == [] = if (null section) then acc else acc ++ [section]
          | (take (length splitStr) str') == splitStr = aux (drop (length splitStr) str') "" (acc ++ [section])
          | otherwise = aux (tail str') (section ++ [head str']) acc


{-|
  Splits `str` on `splitStr` and strips whitespace from each resulting split.
-}
splitAndStrip :: String -> String -> [String]
splitAndStrip splitStr str =
  map strip . split splitStr $ str

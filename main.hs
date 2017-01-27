import System.Random
import Data.String.Utils

contains :: [String] -> String -> Bool
contains list str
  | null list = False
  | (head list) == str = True
  | otherwise = contains (tail list) str

split :: String -> String -> [String]
split splitStr str = aux str "" []
  where aux str section acc
          | str == [] = if (null section) then acc else acc++[section]
          | (take (length splitStr) str) == splitStr = aux (drop (length splitStr) str) "" (acc++[section])
          | otherwise = aux (tail str) (section++[head str]) acc

makeFeedback :: String -> String -> String
makeFeedback correctAnswer userAnswer =
  let correctWords = words correctAnswer
      userWords = words userAnswer
      answerNeedsNoun = (contains ["der", "die", "das"] (correctWords !! 0)) &&
                        (length correctWords) == 2
      answerHasArticle = (contains ["der", "die", "das"] (userWords !! 0))
      correctAnswerStr = "Die richtige Antwort ist \"" ++ correctAnswer ++ "\"."
  in if answerNeedsNoun
     then if answerHasArticle
          then let [correctArticle, correctNoun] = correctWords
                   [userArticle, _] = userWords
                   wrongArticleStr =  "Du hast den falschen Artikel gegeben."
               in if userArticle == correctArticle
                  then correctAnswerStr
                  -- Nouns with "keit", "ung", "shaft" endings are always feminine.
                  else if any (endswith correctNoun) ["keit", "ung", "shaft"]
                       then wrongArticleStr ++ " Achtung! Substantive, die in \"-keit\", \"-ung\", oder \"-shaft\" enden, sind feminin."
                       else wrongArticleStr ++ " Die richtige Artikel ist \"" ++ correctArticle ++ "\"."
          else "Du hast den Artikel vergessen! " ++ correctAnswerStr
     else "Falsch. " ++ correctAnswerStr


quiz :: [[String]] -> IO ()
quiz mappings = do
  questionIndex <- randomRIO (0, length mappings) :: IO Int
  let [englishPhrase, germanPhrase] = mappings !! questionIndex
  putStr (englishPhrase ++ " => ")
  userAnswer <- getLine
  if strip userAnswer == germanPhrase
    then putStr ("Richtig!\n\n")
    else putStr ((makeFeedback germanPhrase userAnswer) ++ "\n\n")
  quiz mappings

main :: IO ()
main = do
  contents <- readFile "words.txt"
  -- Extract English -> German word mappings from raw input.
  let mappings = (map (map strip)) . (map (Main.split "=>")) . lines $ contents
  quiz mappings


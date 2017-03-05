module Main where

import Data.String.Utils (endswith, join, strip)
import Data.List.Split (splitOn)
import System.Console.ANSI (clearScreen)
import System.Random (randomRIO)

import UserDataLoader (loadUserData)
import StringUtils (contains, splitAndStrip)
import VocabLoader (loadVocab)


{-|
  Technically, this only returns true for the subset of noun phrases where the
  noun is preceded by an article. But yolo.
-}
isNounPhrase :: String -> Bool
isNounPhrase str =
  let wordList = words str
  in (length wordList) == 2 &&
     (contains ["der", "die", "das"] (wordList !! 0))


hasArticle :: [String] -> Bool
hasArticle wordList = (not . null $ wordList) &&
                      (contains ["der", "die", "das"] (wordList !! 0))


{-
  TODO: - Convert English feedback to German.
        - After the article is wrong, it doesn't tell you if the word is wrong
          too.
        - Maybe instead of returning strings from each of the feedback methods,
          you should define your own data type that has each possible mistake as
          a value (i.e., NoArticle, WrongArticle, WrongNoun, etc.).
-}


makeArticleFeedback :: String -> String -> Maybe String
makeArticleFeedback userAnswer correctAnswer =
  let userWords = words userAnswer
      correctWords = words correctAnswer
  in
    if (length userWords) /= 2
    then Just "Your answer is in the wrong format."
    else if (hasArticle userWords) then
      let [correctArticle, correctNoun] = correctWords
          [userArticle, _] = userWords
          wrongArticleStr =  "Du hast den falschen Artikel gegeben."
      in if userArticle == correctArticle
         then Nothing
         -- Nouns that end with "-keit", "-ung", "-shaft" are always feminine.
         else if any (endswith correctNoun) ["keit", "ung", "shaft"]
              then Just (wrongArticleStr ++ " Achtung! Substantive, die in \"-keit\", \"-ung\", oder \"-shaft\" enden, sind feminin.")
              else Just (wrongArticleStr ++ " Die richtige Artikel ist \"" ++ correctArticle ++ "\".")
    else Just "Du hast den Artikel vergessen! "


makeNounFeedback :: String -> String -> Maybe String
makeNounFeedback userAnswer correctAnswer =
  let userWords = words userAnswer
      correctWords = words correctAnswer
  in
    if (length userWords == 2)
    then if ((userWords !! 1) == (correctWords !! 1))
          then Nothing
          else Just "Falsch."
    else Just "Your answer isn't in the form of a noun phrase."


makeNounPhraseFeedback :: String -> String -> Maybe String
makeNounPhraseFeedback userAnswer correctAnswer =
  let articleFeedback = makeArticleFeedback userAnswer correctAnswer
      nounFeedback = makeNounFeedback userAnswer correctAnswer
  in
    case (articleFeedback, nounFeedback) of
      (Just a, Nothing) -> Just a
      (_, Just b) -> Just b -- Noun feedback trumps article feedback.
      (Nothing, Nothing) -> Just "THIS SHOULD NEVER BE RETURNED!"


makeFeedback :: String -> String -> Maybe String
makeFeedback userAnswer correctAnswer
  | strip userAnswer == correctAnswer = Nothing
  | otherwise = let correctAnswerStr = "Die richtige Antwort ist \"" ++ correctAnswer ++ "\"."
                in if (isNounPhrase correctAnswer)
                   then case (makeNounPhraseFeedback userAnswer correctAnswer) of
                          Just feedback -> Just (feedback ++ " " ++ correctAnswerStr)
                          Nothing -> Nothing
                   else Just ("Falsch. " ++ correctAnswerStr)


userAck :: IO ()
userAck = do
  putStr "Press ENTER to continue\n"
  -- Wait for the user to acknowledge the feedback before continuing.
  _ <- getLine
  return ()


{-|
  Quizzes the specific phrase at `questionIndex` in `mappings`.
-}
quizIndex :: [[String]] -> [[String]] -> Int -> IO ()
quizIndex userData mappings questionIndex = do
  let [englishPhrase, germanPhrase] = mappings !! questionIndex
  putStr (englishPhrase ++ " => ")
  userAnswer <- getLine
  case makeFeedback userAnswer germanPhrase of
    Just feedback -> do
      putStr (feedback ++ "\n\n")
      userAck
      -- Clear the screen and ask the same question again.
      clearScreen
      quizIndex userData mappings questionIndex
    Nothing -> putStr "Richtig!\n\n"


recordIncorrectInput :: [[String]] -> Int -> [[String]]
recordIncorrectInput userData questionIndex =
  userData
  {-
    Splice a new list together (begin ++ [(thing, val+1)] ++ end).
    Maybe switch to tuples.
  -}


{-|
  Quizzes random phrases from `mappings` without ever stopping.
-}
quiz :: [[String]] -> [[String]] -> IO ()
quiz userData mappings = do
  clearScreen
  questionIndex <- randomRIO (0, length mappings) :: IO Int
  quizIndex userData mappings questionIndex
  userAck
  quiz userData mappings


main :: IO ()
main = do
  clearScreen
  chapters <- loadVocab
  userData <- loadUserData "doobs"  -- TODO: Have them specify user name
  putStr ("Welches Kapitel (A, B, 1-" ++ (show . (+(-2)) . length $ chapters) ++ ", oder alles)? ")
  choice <- getLine
  let cleanChoice = strip choice
  if cleanChoice == "alles"
    then quiz userData (join [] chapters)
    else do
    let chapterIndex = case cleanChoice of
                       "A" -> 0
                       "B" -> 1
                       -- +2 for "A"/"B", -1 for zero-based indexing.
                       a -> (read a) + 1
    quiz userData (chapters !! chapterIndex)
  return ()

-- TODO: Make these imports qualified.
import Data.String.Utils
import Data.List.Split
import System.Console.ANSI
import System.Random


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
quizIndex :: [[String]] -> Int -> IO ()
quizIndex mappings questionIndex = do
  let [englishPhrase, germanPhrase] = mappings !! questionIndex
  putStr (englishPhrase ++ " => ")
  userAnswer <- getLine
  case makeFeedback userAnswer germanPhrase of
    Just feedback -> do
      putStr (feedback ++ "\n\n")
      userAck
      -- Clear the screen and ask the same question again.
      clearScreen
      quizIndex mappings questionIndex
    Nothing -> putStr "Richtig!\n\n"


{-|
  Quizzes random phrases from `mappings` without ever stopping.
-}
quiz :: [[String]] -> IO ()
quiz mappings = do
  clearScreen
  questionIndex <- randomRIO (0, length mappings) :: IO Int
  quizIndex mappings questionIndex
  userAck
  quiz mappings


main :: IO ()
main = do
  clearScreen
  putStr "Loading words...\n"
  contents <- readFile "words.txt"
  clearScreen
  -- Extract English -> German word mappings from raw input.
  -- Split each line on "=>", then strip the leading/trailing whitespace on
  -- every string, and then split on empty lines to separate each list of
  -- mappings into their respective chapters.
  let chapters = splitOn [[]] . (map (map strip)) . (map (Main.split "=>")) . lines $ contents
  putStr ("Welches Kapitel (A, B, 1-" ++ (show . (\x -> x - 2) . length $ chapters) ++ ", oder alles)? ")
  choice <- getLine
  let cleanChoice = strip choice
  if cleanChoice == "alles"
    then quiz (join [] chapters)
    else do
    let chapterIndex = case cleanChoice of
                       "A" -> 0
                       "B" -> 1
                       -- +2 for "A"/"B", -1 for zero-based indexing.
                       a -> (read a) + 1
    quiz (chapters !! chapterIndex)
  return ()


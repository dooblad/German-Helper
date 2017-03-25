module Main where

import Data.List (groupBy, sortBy)
import Data.String.Utils (endswith, join, strip)
import System.Console.ANSI (clearScreen)
import System.Random (randomRIO)
import System.IO (hPutStrLn, withFile, IOMode (WriteMode))
import System.Exit (exitWith, ExitCode (ExitSuccess))

import UserDataLoader (loadUserData, UserData)
import StringUtils (contains)
import VocabLoader (loadVocab, Vocab)


exitCommand :: String
exitCommand = "<EXIT>"


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
quizIndex :: UserData -> Maybe Int -> Vocab -> Int -> IO UserData
quizIndex userData chapterNum mappings questionIndex = do
  let (_, questionId, englishPhrase, germanPhrase) = mappings !! questionIndex
  putStr (englishPhrase ++ " => ")
  userAnswer <- getLine
  if userAnswer == exitCommand
    then do writeUserData "doobs" userData   -- TODO: NO HRADCODE USER
            exitWith ExitSuccess
    else case makeFeedback userAnswer germanPhrase of
           Just feedback -> do
             let newUserData = recordAnswer False userData questionId
             putStr (feedback ++ "\n\n")
             userAck
             -- Clear the screen and ask the same question again.
             clearScreen
             quizIndex newUserData chapterNum mappings questionIndex
           Nothing -> do
             putStr "Richtig!\n\n"
             return (recordAnswer True userData questionId)


recordAnswer :: Bool -> UserData -> Int -> UserData
recordAnswer isCorrect userData questionId =
  let (incorrect, total) = userData !! questionId
      newEntry = (if isCorrect then incorrect else incorrect + 1, total + 1)
  in take questionId userData ++ [newEntry] ++ drop (questionId + 1) userData


createDistribution :: UserData -> [Double]
createDistribution userData = scanl1 (+) . map toPercentage $ userData
  where toPercentage (a, b) = (fromIntegral a) / (fromIntegral b)


findCDFIndex :: Double -> [Double] -> Maybe Int
findCDFIndex weight cdf =
  let cdfLength = length cdf
      aux i =
        if i >= cdfLength then Nothing
        else
          let lowerBound = if i == 0
                           then 0
                           else cdf !! (i - 1)
              upperBound = cdf !! i
          in
            if weight >= lowerBound && weight < upperBound
            then Just i
            else aux (i + 1)
  in aux 0


vocabForChapter :: Vocab -> Maybe Int -> Vocab
vocabForChapter vocab Nothing = vocab
vocabForChapter vocab (Just chapterNum) = sortBy compareId . filter isCorrectChapter $ vocab
  where isCorrectChapter (chapterNum', _, _, _) = chapterNum == chapterNum'
        compareId (_, idA, _, _) (_, idB, _, _) = idA `compare` idB


nextQuestionIndex :: UserData -> Maybe Int -> Vocab -> IO Int
nextQuestionIndex userData chapterNum vocab = do
  let filteredVocab = vocabForChapter vocab chapterNum
  let (_, firstId, _, _) = (head filteredVocab)
  let filteredUserData = take (length filteredVocab) . drop firstId $ userData
  let cdf = createDistribution (filteredUserData)
  let maxWeight = last cdf
  weightChoice <- randomRIO (0, maxWeight) :: IO Double
  case (findCDFIndex weightChoice cdf) of
    Just a -> return (a + firstId)
    Nothing -> error "Couldn't find question index from cumulative distribution scan."


{-|
  Quizzes random phrases from `mappings` without ever stopping.
-}
quiz :: UserData -> Maybe Int -> Vocab -> IO UserData
quiz userData chapterNum vocab = do
  clearScreen
  questionIndex <- nextQuestionIndex userData chapterNum vocab
  newUserData <- quizIndex userData chapterNum vocab questionIndex
  userAck
  quiz newUserData chapterNum vocab


chapterIndex :: String -> Maybe Int
chapterIndex choice =
  case choice of
    "alles" -> Nothing
    "A" -> Just 0
    "B" -> Just 1
    -- +2 for "A"/"B", -1 for zero-based indexing.
    a -> Just ((read a) + 1)


writeUserData :: String -> UserData -> IO ()
writeUserData fileName userData = do
  withFile (fileName ++ ".dat") WriteMode
    (\handle -> hPutStrLn handle (show userData))


printIntro :: IO ()
printIntro = putStrLn "Type <EXIT> at any point to quit"


numChapters :: Vocab -> Int
numChapters vocab =
  (+(-2)) . length . groupBy vocabChapterEqual . sortBy vocabChapterCompare $ vocab
  where vocabChapterCompare (a, _, _, _) (b, _, _, _) = a `compare` b
        vocabChapterEqual a b = vocabChapterCompare a b == EQ

main :: IO ()
main = do
  clearScreen
  vocab <- loadVocab
  userData <- loadUserData vocab "doobs"  -- TODO: Have them specify user name.
  printIntro
  putStr ("Welches Kapitel (A, B, 1-" ++ (show . numChapters $ vocab) ++ ", oder alles)? ")
  choice <- getLine
  let cleanChoice = strip choice
  _ <- quiz userData (chapterIndex cleanChoice) vocab
  return ()

module UserDataLoader where

import System.Directory
import System.IO (hPutStrLn, withFile, IOMode (WriteMode))

import VocabLoader (Vocab)

-- (Number incorrect, Total attempts)
type UserData = [(Int, Int)]


loadUserData :: Vocab -> String -> IO UserData
loadUserData vocab userName = do
  -- TODO: Create directory for user data (use `createDirectoryIfMissing`)
  -- Use `listDirectory` to get all user data entries and create entry if
  -- missing.
  putStr "Loading user data...\n"
  let fileName = userName ++ ".dat"
  userFileExists <- doesFileExist fileName
  if userFileExists
    then do return ()
    else do initUserFile vocab fileName
  userData <- readFile fileName
  return (read userData)


initUserFile :: Vocab -> String -> IO ()
initUserFile vocab fileName = withFile fileName WriteMode
  (\handle -> hPutStrLn handle (show (initWordCounts vocab)))


initWordCounts :: Vocab -> UserData
initWordCounts vocab = map (\_ -> (1, 2)) vocab

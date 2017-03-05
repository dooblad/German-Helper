module UserDataLoader where

import System.Directory

import StringUtils (splitAndStrip)

loadUserData :: String -> IO ([[String]])
loadUserData userName = do
  -- TODO: Create directory for user data (use `createDirectoryIfMissing`)
  -- Use `listDirectory` to get all user data entries and create entry if
  -- missing.
  putStr "Loading user data...\n"
  let userFileName = userName ++ ".dat"
  userFileExists <- doesFileExist userFileName
  if userFileExists
    then do return ()
    else do initUserFile userFileName
  userData <- readFile userFileName
  return (getFrequencies userData)

initUserFile :: String -> IO ()
initUserFile userFileName = do
  -- Create the file.
  _ <- writeFile userFileName ""
  return ()

getFrequencies :: String -> [[String]]
getFrequencies userData = map (splitAndStrip ",") . lines $ userData

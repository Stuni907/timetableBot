{-# LANGUAGE MultiParamTypeClasses #-}
module PersistFile where

import Data.Text (Text)
import Data.List (find)
import Telegram.Bot.API (ChatId(..))
import Data.List (nub)
import System.Directory
import Persist (TimettableUrl,Entry,PersistSetup, PersistCreateOperation, PersistQueryOperation, PersistDeleteOperation, initDB,addEntry, getEntries, getEntry, removeEntry,getFileName, addReminder, getAllReminders, removeReminder)


logEntries :: Int -> IO ()
logEntries n = do
    putStrLn $ "Entries: " ++ show n

instance PersistSetup where
  getFileName = "./registeredUsers.txt"
  initDB = do
    fileExists <- doesFileExist getFileName
    if fileExists
      then putStrLn "File exists"
      else writeFile getFileName $ show ([] :: [(Int, TimettableUrl)])


instance PersistCreateOperation where
  addEntry entry = do
    entries <- getEntries
    let newEntries = entry : entries
    logEntries (length newEntries)
    writeFile getFileName $ show $ nub newEntries
  addReminder chatId = fail "Use Database instead"

instance PersistQueryOperation where
  getEntries = do
    content <- readFile getFileName
    let entries = read content :: [(Int, TimettableUrl)]
    logEntries (length entries)
    return entries
  getEntry chatId = do
    entries <- getEntries
    let entry = find (\x -> chatId == fst x) entries
    return $ fmap snd entry
  getAllReminders = fail "Use Database instead"

instance PersistDeleteOperation where
  removeEntry chatId = do
    entries <- getEntries
    let newEntries = filter (\x -> chatId /= fst x) entries
    writeFile getFileName $ show newEntries
  removeReminder chatId = fail "Use Database instead"

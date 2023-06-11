{-# LANGUAGE OverloadedStrings,MultiParamTypeClasses #-}
module PersistDatabase
  ( RegisteredUsers(..)
  , checkDatabaseFileExists
  , createRegisteredUsersTable
  , initDB
  , addEntry
  , addReminder
  , getEntries
  , getEntry
  , getAllReminders
  , removeEntry
  , removeReminder
  ) where

import Data.Text ( Text, pack )
import Control.Exception (try, SomeException(..))
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.Directory (doesFileExist)
import Telegram.Bot.API (ChatId(..))
import Persist (TimettableUrl,Entry,PersistSetup, PersistCreateOperation, PersistQueryOperation, PersistDeleteOperation, initDB,addEntry, getEntries, getEntry, removeEntry,getFileName, addReminder, getAllReminders, removeReminder)
import System.IO

data RegisteredUsers = RegisteredUsers Int Int String Int deriving (Show)

instance FromRow RegisteredUsers where
    fromRow = RegisteredUsers <$> field <*> field <*> field <*> field

checkDatabaseFileExists :: FilePath -> IO Bool
checkDatabaseFileExists dbFile = doesFileExist dbFile

createRegisteredUsersTable :: Query
createRegisteredUsersTable = "CREATE TABLE IF NOT EXISTS RegisteredUsers (RegisteredUserId INTEGER PRIMARY KEY, ChatId Integer NOT NULL UNIQUE, TimetableUrl TEXT, Reminder INTEGER DEFAULT 0)"

instance PersistSetup where
    getFileName = "telegramBot.db"
    initDB = do
        let dbName = getFileName
        conn <- open dbName
        execute_ conn createRegisteredUsersTable
        close conn

instance PersistCreateOperation where
    addEntry newEntry = do
        let dbName = getFileName
        result <- try $ do
            conn <- open dbName
            execute conn "INSERT INTO RegisteredUsers (ChatId, TimetableUrl, Reminder) VALUES (?, ?, 0)"
                newEntry
            close conn
        case result of
            Left e -> putStrLn $ "Error on inserting row: " ++ show (e :: SomeException)
            Right _ -> putStrLn "Success"
    addReminder chatId = do
        let dbName = getFileName
        result <- try $ do
            conn <- open dbName
            execute conn "UPDATE RegisteredUsers SET Reminder = 1 WHERE ChatId = ?"
                (Only chatId)
            close conn
        case result of
            Left e -> putStrLn $ "Error on inserting row: " ++ show (e :: SomeException)
            Right _ -> putStrLn "Success"

instance PersistQueryOperation where
    getEntries = do
        let dbName = getFileName
        result <- try $ do
            conn <- open dbName
            r <- query_ conn "SELECT * from RegisteredUsers" :: IO [RegisteredUsers]
            let mappedToTuple = map (\(RegisteredUsers _ x y _) -> (x,pack y)) r            
            close conn
            pure mappedToTuple
        case result of
            Left e -> do
                putStrLn $ "Error on getting entries: " ++ show (e :: SomeException)
                pure []
            Right res -> do
                putStrLn "Success"
                return res
    getEntry chatId = do
            let dbName = getFileName
            conn <- open dbName
            r <- query conn "SELECT * from RegisteredUsers WHERE ChatId = ?"
                (Only chatId)
            res r
     where res [] = pure Nothing
           res ((RegisteredUsers _ _ url _):xs) = pure (Just $ pack url)
    getAllReminders = do
        let dbName = getFileName
        conn <- open dbName
        r <- query_ conn "SELECT * from RegisteredUsers WHERE Reminder = 1" :: IO [RegisteredUsers]
        let mappedToTuple = map (\(RegisteredUsers _ x y _) -> (x, pack y)) r
        pure mappedToTuple

instance PersistDeleteOperation where
    removeEntry chatId = do
        let dbName = getFileName
        conn <- open dbName
        execute conn "DELETE FROM RegisteredUsers WHERE ChatId = ?"
            (Only chatId)
        close conn
    removeReminder chatId = do
        let dbName = getFileName
        conn <- open dbName
        execute conn "UPDATE RegisteredUsers SET Reminder = 0 WHERE ChatId = ?"
            (Only chatId)
        close conn
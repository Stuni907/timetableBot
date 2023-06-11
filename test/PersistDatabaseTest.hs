{-# LANGUAGE OverloadedStrings,MultiParamTypeClasses #-}
module PersistDatabaseTest where

import Test.Hspec
import Persist (TimettableUrl,Entry,PersistSetup, PersistCreateOperation, PersistQueryOperation, PersistDeleteOperation, initDB,addEntry, getEntries, getEntry, removeEntry,getFileName, addReminder, getAllReminders, removeReminder)
import PersistDatabase (checkDatabaseFileExists, initDB,addEntry,RegisteredUsers(..),getEntries,getEntry)
import System.Directory (removeFile, doesFileExist)
import Database.SQLite.Simple 
import Database.SQLite.Simple.FromRow (FromRow)
import Persist (Entry, TimettableUrl)
import Data.Text (Text)
import Control.Exception (try, SomeException(..))


runDBTests :: Spec
runDBTests = do
  describe "checkDatabaseFileExists" $ do
    it "returns True if the database file exists" $ do
      _ <- setUpTest
      _ <- initDB
      exists <- checkDatabaseFileExists "telegramBot.db"
      exists `shouldBe` True
    it "returns False if the database file does not exist" $ do
      fileExists <- checkDatabaseFileExists "nonexistent.db"
      fileExists `shouldBe` False
  describe "createRegisteredUsersTable" $ do
    it "creates the RegisteredUsers table if it does not exist" $ do
      _ <- setUpTest
      _ <- initDB 
      withConnection "telegramBot.db" $ \conn -> do
        [Only tableCount] <- query_ conn "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name='RegisteredUsers'" 
        (tableCount) `shouldBe` (1::Int)


runAddEntryTests :: Spec
runAddEntryTests = do
  describe "addEntry" $ do
    it "successfully adds a new entry" $ do
      _ <- setUpTest
      let entry = (1, "example.com")
      _ <- addEntry entry
      -- Check if the entry exists in the database
      entries <- getEntries
      let entryExists = any (\(chatId, url) -> chatId == 1) entries
      entryExists `shouldBe` True
    it "does not add a duplicate entry" $ do
        _ <- setUpTest
        let entry = (1, "example.com")
        -- Add the entry for the first time
        addEntry entry
        -- Add the same entry again
        addEntry entry
        -- Check if the entry is only present once in the database
        entries <- getEntries
        let count = length $ filter (\(chatId, _) -> chatId == 1) entries
        count `shouldBe` 1

    it "adds an entry with a different ChatId" $ do
      _ <- setUpTest
      let entry1 = (1, "example.com")
      let entry2 =(2,"example.com")
      -- Add the first entry
      addEntry entry1
      -- Add the second entry with a different ChatId
      addEntry entry2
      -- Check if both entries exist in the database
      entries <- getEntries
      let entry1Exists = any (\(chatId, url) -> chatId == 1 && url == "example.com") entries
      let entry2Exists = any (\(chatId, url) -> chatId == 2 && url == "example.com") entries
      entry1Exists `shouldBe` True
      entry2Exists `shouldBe` True

runGetEntryTests :: Spec
runGetEntryTests = do
  describe "getEntry" $ do
    it "returns Nothing for a non-existing chatId" $ do
      _ <- setUpTest
      let chatId = 12345
      entry <- getEntry chatId
      entry `shouldBe` Nothing
    it "returns Just entry for an existing chatId" $ do
      _ <- setUpTest
      let entry = (1, "example.com")
      addEntry entry
      maybeUrl <- getEntry 1
      case maybeUrl of
        Just retrievedUrl -> retrievedUrl `shouldBe` "example.com"

runRemoveEntryTests :: Spec
runRemoveEntryTests = do
  describe "removeEntry" $ do
    it "removes an existing entry" $ do
      _ <- setUpTest
      let entry = (1, "example.com")
      addEntry entry
      removeEntry 1
      result <- getEntry 1
      result `shouldBe` Nothing

    it "does not remove a non-existing entry" $ do
      _ <- setUpTest
      let entry = (1, "example.com")
      addEntry entry
      removeEntry 2
      result <- getEntry 1
      result `shouldBe` Just "example.com"

runReminderTests :: Spec
runReminderTests = do
  describe "addReminder" $ do
    it "adds a reminder for an existing chatId" $ do
      _ <- setUpTest
      let entry = (1, "example.com")
      addEntry entry
      addReminder 1
      reminders <- getAllReminders
      length reminders `shouldBe` 1

    it "does not add a reminder for a non-existing chatId" $ do
      _ <- setUpTest
      addReminder 12345
      reminders <- getAllReminders
      length reminders `shouldBe` 0

  describe "getAllReminders" $ do
    it "returns all registered reminders" $ do
      _ <- setUpTest
      let entry1 = (1, "example.com")
      let entry2 = (2, "example.org")
      addEntry entry1
      addEntry entry2
      addReminder 1
      addReminder 2
      reminders <- getAllReminders
      length reminders `shouldBe` 2

    it "returns an empty list when there are no reminders" $ do
      _ <- setUpTest
      reminders <- getAllReminders
      reminders `shouldBe` []

  describe "removeReminder" $ do
    it "removes a reminder for an existing chatId" $ do
      _ <- setUpTest
      let entry = (1, "example.com")
      addEntry entry
      addReminder 1
      removeReminder 1
      reminders <- getAllReminders
      length reminders `shouldBe` 0

    it "does not remove a reminder for a non-existing chatId" $ do
      _ <- setUpTest
      removeReminder 12345
      reminders <- getAllReminders
      length reminders `shouldBe` 0
 


setUpTest :: IO()
setUpTest = do
  fileExists <- doesFileExist "telegramBot.db"
  _ <- if fileExists then  removeFile "telegramBot.db" else  return ()
  _ <- initDB
  return ()

cleanUpTest :: IO()
cleanUpTest = do
  fileExists <- doesFileExist "telegramBot.db"
  if fileExists then  removeFile "telegramBot.db" else  return ()
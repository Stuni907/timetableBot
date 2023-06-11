{-# LANGUAGE OverloadedStrings #-}
module BotLogicTest where


import Test.Hspec
import qualified Data.Text as Text
import Persist (TimettableUrl,Entry,PersistSetup, PersistCreateOperation, PersistQueryOperation, PersistDeleteOperation, initDB,addEntry, getEntries, getEntry, removeEntry,getFileName, addReminder, getAllReminders, removeReminder)
import PersistDatabase (checkDatabaseFileExists, initDB,addEntry,RegisteredUsers(..),getEntries,getEntry)
import System.Directory (removeFile, doesFileExist)
import Database.SQLite.Simple 
import Database.SQLite.Simple.FromRow (FromRow)
import Control.Exception (try, SomeException(..))
import BotLogic

runBotLogicTests :: Spec
runBotLogicTests = do
    describe "registerWithUrl" $ do
      it "returns 'Registrierung erfolgreich! Probier doch mal /next aus.' if the URL starts with 'https://bariapi.fhnw.ch/cit_auxilium/prod/api/Calendar/'" $ do
        _<-setUpTestWithEntry
        let result = registerWithUrl 1 "https://bariapi.fhnw.ch/cit_auxilium/prod/api/Calendar/f9341345gsd9f4f62345234b4ec.ics?correlationId=c7c234209c32-245a-4374-aa467sfgs7e42b&language=DE/"
        result >>= \r -> r `shouldBe` "Registrierung erfolgreich! Probier doch mal /next aus."
      it "returns 'Das ist keine gültige URL. Bitte versuche es erneut." $ do
        _<-setUpTestWithEntry
        let result = registerWithUrl 1 "https://example.org"
        result >>= \r -> r `shouldBe` "Das ist keine gültige URL. Bitte versuche es erneut."
      it "User muss sich zuerst registrieren" $ do
        _ <- setUpTestWithEntry
        let result = getAsText 0 (\_ -> (pure (Just "T1"))) (\_ -> "T2")
        result >>= \r -> r `shouldBe` "Du bist noch nicht registriert. Registriere dich mit /register"
      it "User ist registiert" $ do
        _ <- setUpTestWithEntry
        let result = getAsText 1 (\_ -> (pure (Just "T1"))) (\_ -> "T2")
        result >>= \r -> r `shouldBe` "T2"
      it "User is in registration process" $ do
        _ <- setUpTestWithEntry
        let result = isInRegistrationProcess 1
        result >>= \r -> r `shouldBe` True
      it "User is not in registration process" $ do
        _ <- setUpTestWithEntry
        let result = isInRegistrationProcess 2
        result >>= \r -> r `shouldBe` False

setUpTestWithEntry :: IO()
setUpTestWithEntry = do
  fileExists <- doesFileExist "telegramBot.db"
  _ <- if fileExists then  removeFile "telegramBot.db" else  return ()
  _ <- initDB
  let entry1 = (1, "")
      entry2 = (2, "example.org")
  _ <- addEntry entry1
  _ <- addEntry entry2
  return ()

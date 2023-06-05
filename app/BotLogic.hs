{-# LANGUAGE OverloadedStrings #-}
module BotLogic where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.IO.Class (liftIO)

import Persist
import PersistDatabase
import Calendar
import EventFormater
import Data.Maybe (catMaybes)


-- | Call at start
initPersist :: IO ()
initPersist = do
    initDB
    pure ()

-- | Start the bot (/start)
welcomeMessage :: Text
welcomeMessage = Text.unlines
      [
        "Hallo! Ich bin dein Lektionen-Bot."
      , ""
      , "Mithilfe von /next kannst du mich fragen, was und wo deine nächste Lektion ist."
      , "<Weitere Commands>"
      , ""
      , "Zuerst solltest du dich aber registrieren, dass ich deinen Stundenplan kenne."
      , "Starte die Registrierung mit /register"
      ]

-- | Message after /register
registerMessage :: Text
registerMessage = Text.unlines
      [
        "Sende mir doch bitte die URL deines Stundenplans zu."
      , "Du findest die Url des Stundenplans wenn du unter https://auxilium.webapps.fhnw.ch/student/timetable auf \"Abonnieren\" klickst."
      , "Die Url startet mit \"https://bariapi.fhnw.ch...\""
      , "Kopiere sie und sende sie mir zu."
      ]

-- | Unregister a user (/unregister)
unregisterUser :: Int -> IO Text
unregisterUser chatId = do
    removeEntry chatId
    pure "Schade dass du dich abgemeldet hast. Hoffentlich bis bald wieder!"


-- | Add a user to the registration process (/register)
addWaitForCompleteRegistration :: Int -> IO Text
addWaitForCompleteRegistration chatId = do
    removeEntry chatId
    addEntry (chatId, "")
    pure registerMessage

-- | Complete the registration process for a user
-- | Please check if the user is in the registration process before calling this function
completeRegistration :: Int -> Text -> IO ()
completeRegistration chatId url = do
    removeEntry chatId
    addEntry (chatId, url)
    pure ()

-- | Check if a user is in the registration process
isInRegistrationProcess :: Int -> IO Bool
isInRegistrationProcess chatId = do
    url <- getEntry chatId
    case url of
        Nothing -> pure False
        Just url -> pure $ Text.null url

-- | Register a user with a url (Message after /register)
registerWithUrl :: Int -> Text -> IO Text
registerWithUrl id url = do
      inProcess <- isInRegistrationProcess id
      if inProcess then
        if Text.isPrefixOf "https://bariapi.fhnw.ch/cit_auxilium/prod/api/Calendar/" url then do
          completeRegistration id url
          pure "Registrierung erfolgreich! Probier doch mal /next aus."
        else do
          pure "Das ist keine gültige URL. Bitte versuche es erneut."
      else do
        pure welcomeMessage

startToRemindeUser :: Int -> IO Text
startToRemindeUser id = do
    addReminder id
    pure "Ab jetzt werde ich dich 10 Minuten vor jeder Lektion erinnern.\n Deaktivere es mit /stop"

stopToRemindeUser :: Int -> IO Text
stopToRemindeUser id = do
    removeReminder id
    pure "Ich werde dich nicht mehr erinnern."

getReminderMessages :: IO [(Int, Text)]
getReminderMessages = do
    users <- getAllReminders
    catMaybes <$> mapM (\(id, url) -> do
        msg <- getReminderMessage $ Text.unpack url
        case msg of
            Nothing -> pure Nothing
            Just msg -> pure $ Just (id, msg)
        ) users

getReminderMessage :: String -> IO (Maybe Text)
getReminderMessage url = do
    event <- getEventIfStartsIn10Minutes url
    case event of
        Nothing -> pure Nothing
        Just event -> pure $ Just $ formatEvent event


-- | *************
getAsText :: Int -> (String -> IO (Maybe a)) -> (a -> Text) -> IO Text
getAsText id get parse = do
                      url <- getEntry id
                      case url of
                        Nothing -> pure "Du bist noch nicht registriert. Registriere dich mit /register"
                        Just url -> do
                          event <- get $ Text.unpack url
                          case event of
                            Nothing -> pure "Keine Vorlesung gefunden."
                            Just event -> pure $ parse event

getNextEventAsText :: Int -> IO Text
getNextEventAsText id = getAsText id getNextEvent formatEvent

getNowEventAsText :: Int -> IO Text
getNowEventAsText id = getAsText id getNowEvent formatEvent

getTodayEventsAsText :: Int -> IO Text
getTodayEventsAsText id = getAsText id getTodayEvents formatEvents

getTechnicalNextEventAsText :: Int -> IO Text
getTechnicalNextEventAsText id = getAsText id getNextEvent formatTechnicalEvent
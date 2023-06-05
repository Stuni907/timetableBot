{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative

import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.List (find, nub)

import Control.Monad.IO.Class (liftIO)

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser
import Telegram.Bot.API.Methods.SetMyCommands

import Persist
import PersistDatabase 
import BotLogic
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock.System
import Data.Time


newtype Model = Model
  {
    waitForLink :: [ChatId]
  }

initialModel :: Model
initialModel = Model
  {
    waitForLink = []
  }

data Action
  =  Start
  | Register Int
  | Next Int
  | Now Int
  | Today Int
  | RegisterUrl Int TimettableUrl
  | Unregister Int
  | Reminder Int
  | Stop Int
  deriving (Show)

timetableBot :: BotApp Model Action
timetableBot = BotApp
  {
    botInitialModel = initialModel
  , botAction = flip updateToAction
  , botHandler = handler
  , botJobs = [
      BotJob {botJobSchedule="* * * * *", botJobTask=reminders}
  ]
  }
  where
    reminders :: model -> Eff Action model
    reminders model = do
      model <# do
        messagesToSend <- liftIO getReminderMessages
        mapM_ (\(chatId, msg) -> sendMessageTo (ChatId $ fromIntegral chatId) msg) messagesToSend
        
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ update =
      (parseUpdate $
          RegisterUrl parsedChatId <$> plainText
      <|> Start                   <$ command "start"
      <|> Register parsedChatId   <$ command "register"
      <|> Unregister parsedChatId <$ command "unregister"
      <|> Next parsedChatId       <$ command "next"
      <|> Now parsedChatId        <$ command "now"
      <|> Today parsedChatId      <$ command "today"
      <|> Reminder parsedChatId   <$ command "reminder"
      <|> Stop parsedChatId       <$ command "stop"
      ) update
      where parsedChatId = getIdFromChatId (updateChatId update)

handler :: Action -> Model -> Eff Action Model
handler action model = model <# handleAction action

handleAction :: Action -> BotM ()-- -> Model -> Eff Action Model
handleAction action = case action of
  -- Just send a welcome message
  Start -> replyText welcomeMessage
  -- Start register process and expect the next message to be the url
  Register id -> do
    msg <- liftIO $ addWaitForCompleteRegistration id
    replyText msg
  -- If it is expected to be the url, add it to the database
  -- else send the start message, because the user is not in the register process
  RegisterUrl id url -> do
    msg <- liftIO $ registerWithUrl id url
    replyText msg
  Unregister id -> do
    msg <- liftIO $ unregisterUser id
    replyText msg
  Next id  -> do
    msg <- liftIO $ getNextEventAsText id
    replyText msg
  Now id  -> do
    msg <- liftIO $ getNowEventAsText id
    replyText msg
  Today id  -> do
    msg <- liftIO $ getTodayEventsAsText id
    replyText msg
  Reminder id -> do
    msg <- liftIO $ startToRemindeUser id
    replyText msg
  Stop id -> do
    msg <- liftIO $ stopToRemindeUser id
    replyText msg
-- | takes a 'ChatId' and extracts the integer ID from it.
getIdFromChatId :: Maybe ChatId -> Int
getIdFromChatId (Just (ChatId id)) = fromInteger id
getIdFromChatId Nothing = 0

sendMessageTo :: ChatId -> Text -> BotM ()
sendMessageTo chatId msg = replyTo (SomeChatId chatId) ReplyMessage { 
                                            replyMessageText = msg,
                                            replyMessageMessageThreadId = Nothing,
                                            replyMessageParseMode = Nothing, 
                                            replyMessageEntities = Nothing,
                                            replyMessageDisableWebPagePreview = Nothing,
                                            replyMessageDisableNotification = Nothing,
                                            replyMessageProtectContent = Nothing,
                                            replyMessageReplyToMessageId = Nothing,
                                            replyMessageAllowSendingWithoutReply = Nothing,
                                            replyMessageReplyMarkup = Nothing
        }

run :: IO ()
run = do
  content <- readFile "token.env"
  let token = pack content
  env <- defaultTelegramClientEnv $ Token token
  startBot_ (conversationBot updateChatId timetableBot) env

main :: IO ()
main = do
  putStrLn "Up and running"
  initPersist
  run
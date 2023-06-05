{-# LANGUAGE OverloadedStrings #-}
module EventFormater where

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Time
import Calendar
import ICalParser
import Data.Time.Clock.POSIX
import Data.Int


formatEvent :: Event -> Text
formatEvent event = do
    let startTime =   addUTCTime (offsetHours event) (posixSecondsToUTCTime (start event))
    let endTime =  addUTCTime (offsetHours event) (posixSecondsToUTCTime (end event))
    let startString = formatTime defaultTimeLocale "%H:%M" startTime
    let endString = formatTime defaultTimeLocale "%H:%M" endTime
    let dateString = formatTime defaultTimeLocale "%d.%m.%Y" startTime
    let locString = loc event
    let descString = desc event
    Text.pack $ startString ++ " - " ++ endString ++ " " ++ dateString ++ "\n" ++ locString ++ "\n" ++ descString


formatEvents :: [Event] -> Text
formatEvents events = Text.intercalate "\n\n" $ map formatEvent events

formatTechnicalEvent :: Event -> Text
formatTechnicalEvent event = do
    let startTime = addUTCTime (offsetHours event) (posixSecondsToUTCTime (start event))
    let endTime =  addUTCTime (offsetHours event) (posixSecondsToUTCTime (start event))
    let startString = formatTime defaultTimeLocale "%H:%M" startTime
    let endString = formatTime defaultTimeLocale "%H:%M" endTime
    let dateString = formatTime defaultTimeLocale "%d.%m.%Y" startTime
    let locString = loc event
    let descString = desc event
    Text.pack $ startString ++ " - " ++ endString ++ " " ++ dateString ++ "\n" ++ locString ++ "\n" ++ descString


secondsToUTCTime :: Integer -> UTCTime
secondsToUTCTime seconds = addUTCTime (fromIntegral seconds) (UTCTime (toEnum 0) 0)
{-# LANGUAGE OverloadedStrings #-}
module Calendar where

import ICalParser
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as Text
import qualified Network.HTTP.Simple as Net
import Data.List (find, sortBy)
import Data.Time.Clock.POSIX
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Calendar
import Data.Time.Clock

getICalData :: String -> IO String
getICalData url = do
    response <- Net.httpLBS (Net.parseRequest_ url)
    return $ normalizeLineEndings $ L8.unpack $ Net.getResponseBody response

normalizeLineEndings :: String -> String
normalizeLineEndings = Text.unpack . Text.replace "\r\n" "\n" . Text.pack

getEvents :: String -> IO [Event]
getEvents url = do
    content <- getICalData url
    let events = case parseCalendar content of
                        Just x -> x
                        Nothing -> []
    return events

sortEvents :: [Event] -> [Event]
sortEvents events = sortBy (\x y -> compare (start x) (start y)) events

removeEventsBeforeNow :: [Event] -> UTCTime -> [Event]
removeEventsBeforeNow events now = filter (\x -> start x > utcTimeToPOSIXSeconds now) events

getNextEvent :: String -> IO (Maybe Event)
getNextEvent url = do
    events <- getEvents url
    now <- getCurrentTime
    let sortedEvents = sortEvents $ removeEventsBeforeNow events now
    let nextEvent = find (\x -> start x > utcTimeToPOSIXSeconds now) sortedEvents
    return nextEvent

getNowEvent :: String -> IO (Maybe Event)
getNowEvent url = do
    events <- getEvents url
    now <- getCurrentTime
    let sortedEvents = sortEvents events
    let nowEvent = find (\x -> start x <= utcTimeToPOSIXSeconds now && end x >= utcTimeToPOSIXSeconds now) sortedEvents
    return nowEvent

getEventIfStartsBetween :: String -> UTCTime -> UTCTime -> IO (Maybe Event)
getEventIfStartsBetween url startInterval endInterval = do
    events <- getEvents url
    let sortedEvents = sortEvents events
    let nowEvent = find (\x -> start x >= utcTimeToPOSIXSeconds startInterval && start x <= utcTimeToPOSIXSeconds endInterval) sortedEvents
    return nowEvent

getEventIfStartsIn10Minutes :: String -> IO (Maybe Event)
getEventIfStartsIn10Minutes url = do
    now <- getCurrentTime
    let fiveMinutes = 10 * 60
    let startInterval = addUTCTime (fiveMinutes - 30) now
    let endInterval = addUTCTime (fiveMinutes + 30) now
    getEventIfStartsBetween url startInterval endInterval

getTodayEvents :: String -> IO (Maybe [Event])
getTodayEvents url = do
    events <- getEvents url
    now <- getCurrentTime
    startOfDay <- getStartOfDay now
    endOfDay <- getEndOfDay now
    let sortedEvents = sortEvents events
    let todayEvents = filter (\x -> start x >= utcTimeToPOSIXSeconds startOfDay && end x <= utcTimeToPOSIXSeconds endOfDay) sortedEvents
    if null todayEvents
        then return Nothing
        else return (Just todayEvents)

getStartOfDay :: UTCTime -> IO UTCTime
getStartOfDay now = do
    let (year, month, day) = toGregorian $ utctDay now
    return $ UTCTime (fromGregorian year month day) 0

getEndOfDay :: UTCTime -> IO UTCTime
getEndOfDay now = do
    let (year, month, day) = toGregorian $ utctDay now
    return $ UTCTime (fromGregorian year month day) 86400
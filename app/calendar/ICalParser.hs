module ICalParser where

import Control.Applicative
import Control.Monad (void)

import Data.Time
import Data.Int
import Data.Time.Clock.POSIX

import Parser

data Event = Event
    { start :: POSIXTime
    , end :: POSIXTime
    , loc :: String
    , desc :: String
    , offsetHours :: NominalDiffTime
    }deriving (Show)



-- Calendar parser
calendar :: Parser [Event]
calendar = do
    beginKeyword "VCALENDAR"
    untilString "TZNAME:CET"
    offsetTime <- timeZoneHours
    untilString "BEGIN:VEVENT"
    l <- eventList offsetTime
    endKeyword "VCALENDAR"
    return l

timeZoneHours :: Parser NominalDiffTime
timeZoneHours = do
    string "TZNAME:CET"
    eol
    string "TZOFFSETFROM:"
    h <- many (sat (/= '\n'))
    case h of
        "+0100" -> return 1
        "+0200" -> return 2
        _ -> return 0

-- Multiple Event parser
eventList :: NominalDiffTime -> Parser [Event]
eventList offsetTime = many (event offsetTime)

-- Event Parser
event :: NominalDiffTime -> Parser Event
event offsetTime = do
    beginKeyword "VEVENT"
    ignoreLine
    end <- eventEnd offsetTime
    ignoreLine
    start <- eventStart offsetTime
    loc <- locationOrOnline
    untilString "SUMMARY"
    sum <- summary
    untilString "END:VEVENT"
    endKeyword "VEVENT"
    return (Event start end loc sum (offsetTime * 3600))

-- Testing calendar parser
test :: IO ()
test = do
    content <- readFile "test.ics"
    let result = parse calendar content
    print result

-- Returns location or "Online" if no location is given
locationOrOnline :: Parser String
locationOrOnline = do
    location
    <|> do
        ignoreLine
        return "Online"


location :: Parser String
location = do
    string "LOCATION:"
    l <- many (sat (/= '\n'))
    eol
    return l

-- Summary parser
summary :: Parser String
summary = do
    string "SUMMARY:"
    s <- many (sat (/= '\n'))
    eol
    return s

-- parse Time
getTime :: String -> UTCTime
getTime dateString = parseTimeOrError True defaultTimeLocale "%Y%m%dT%H%M%S" dateString :: UTCTime

-- Parse event time
eventTime :: String -> Parser UTCTime
eventTime s = do
    string s
    t <- many (sat (/= '\n'))
    eol
    return (getTime t)

-- Parse event start
eventStart :: NominalDiffTime -> Parser POSIXTime
eventStart offsetTime = fmap (toSecondsAndAddTimeDiff offsetTime)  (eventTime "DTSTART;TZID=Europe/Zurich:")

-- Parse event end
eventEnd :: NominalDiffTime -> Parser POSIXTime
eventEnd offsetTime = fmap (toSecondsAndAddTimeDiff offsetTime) (eventTime "DTEND;TZID=Europe/Zurich:")

-- Convert to seconds and add offset
toSecondsAndAddTimeDiff :: NominalDiffTime -> UTCTime -> POSIXTime
toSecondsAndAddTimeDiff offsetTime time = utcTimeToPOSIXSeconds (addUTCTime (-3600 * offsetTime) time)

-- Ignore line
ignoreLine :: Parser (Maybe a)
ignoreLine = do
    many (sat (/= '\n'))
    eol
    return Nothing


-- Parser without consuming input
notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = P $ \input ->
  case parse p input of
    Nothing -> Just ((), input)
    _       -> Nothing

-- End of line parser
eol :: Parser ()
eol = fmap (const ()) (char '\n')

-- Parse until keyword
untilString :: String -> Parser ()
untilString keyword = void $
                    concat <$>
                    many (some (notFollowedBy (string keyword) *> item))

-- Lines with BEGIN:
beginKeyword :: String -> Parser String
beginKeyword s = string "BEGIN:" *> string s <* eol

-- Lines with END:
endKeyword :: String -> Parser String
endKeyword s = string "END:" *> string s <* eol

parseCalendar :: String -> Maybe [Event]
parseCalendar content = case parse calendar content of
    Just (events, _) -> Just events
    Nothing -> Nothing
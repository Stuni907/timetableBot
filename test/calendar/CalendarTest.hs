module CalendarTest where

import Test.Hspec
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.List (sortBy)
import Data.Maybe (isJust)
import Control.Monad.IO.Class (liftIO)
import ICalParser
import Calendar
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Data.Time



instance Eq Event where
  (Event start1 end1 loc1 desc1 offsetHours1) == (Event start2 end2 loc2 desc2 offsetHours2) =
    start1 == start2 && end1 == end2 && loc1 == loc2 && desc1 == desc2 && offsetHours1 == offsetHours2

runCalendarTests :: Spec
runCalendarTests = do
    describe "sortEvents" $
        it "sorts events in ascending order based on start date" $ do
        let event1 = Event
                { start = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 4 6) (timeOfDayToTime (TimeOfDay 6 15 0)))
                , end = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 4 6) (timeOfDayToTime (TimeOfDay 9 0 0)))
                , loc = "FPROD2"
                , desc = "Projekt"
                , offsetHours = 7200
                }
            event2 = Event
                { start = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 10 5) (timeOfDayToTime (TimeOfDay 6 15 0)))
                , end = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 10 5) (timeOfDayToTime (TimeOfDay 9 0 0)))
                , loc = "FPROD3"
                , desc = "Projekt"
                , offsetHours = 7200
                }
            event3 = Event
                { start = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 4 5) (timeOfDayToTime (TimeOfDay 6 15 0)))
                , end = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 4 5) (timeOfDayToTime (TimeOfDay 9 0 0)))
                , loc = "FPROD1"
                , desc = "Projekt"
                , offsetHours = 7200
                }
            events = [event1, event2, event3]
            sortedEvents = sortEvents events
        sortedEvents `shouldBe` [event3, event1, event2]
    describe "sortEvents" $
        it "sorts events in ascending order based on start time" $ do
        let event1 = Event
                { start = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 4 6) (timeOfDayToTime (TimeOfDay 10 15 0)))
                , end = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 4 6) (timeOfDayToTime (TimeOfDay 13 0 0)))
                , loc = "FPROD2"
                , desc = "Projekt"
                , offsetHours = 7200
                }
            event2 = Event
                { start = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023  4 6) (timeOfDayToTime (TimeOfDay 15 15 0)))
                , end = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023  4 6) (timeOfDayToTime (TimeOfDay 18 0 0)))
                , loc = "FPROD3"
                , desc = "Projekt"
                , offsetHours = 7200
                }
            event3 = Event
                { start = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023  4 6) (timeOfDayToTime (TimeOfDay 6 15 0)))
                , end = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023  4 6) (timeOfDayToTime (TimeOfDay 9 0 0)))
                , loc = "FPROD1"
                , desc = "Projekt"
                , offsetHours = 7200
                }
            events = [event1, event2, event3]
            sortedEvents = sortEvents events
        sortedEvents `shouldBe` [event3, event1, event2]
    describe "removeEventsBeforeNow" $
        it "removes events that start before the given current time" $ do
        let now =  (UTCTime (fromGregorian 2023 4 5) (timeOfDayToTime (TimeOfDay 6 15 0)))
            event1 = Event
                    { start = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2022 4 6) (timeOfDayToTime (TimeOfDay 10 15 0)))
                    , end = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 4 6) (timeOfDayToTime (TimeOfDay 13 0 0)))
                    , loc = "FPROD2"
                    , desc = "Projekt"
                    , offsetHours = 7200
                    }
            event2 =  Event
                    { start = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023  4 6) (timeOfDayToTime (TimeOfDay 15 15 0)))
                    , end = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023  4 6) (timeOfDayToTime (TimeOfDay 18 0 0)))
                    , loc = "FPROD3"
                    , desc = "Projekt"
                    , offsetHours = 7200
                    }
            events = [event1, event2]
            remainingEvents = removeEventsBeforeNow events now
        remainingEvents `shouldBe` [event2]

    describe "getStartOfDay" $
        it "returns the start of the day for a given UTC time" $ do
        now <- liftIO getCurrentTime
        startOfDay <- liftIO $ getStartOfDay now
        let (year, month, day) = toGregorian $ utctDay now
        let expected = UTCTime (fromGregorian year month day) 0
        startOfDay `shouldBe` expected
    describe "getEndOfDay" $ 
        it "returns the end of the day for a given UTC time" $ do
        now <- liftIO getCurrentTime
        endOfDay <- liftIO $ getEndOfDay now
        let (year, month, day) = toGregorian $ utctDay now
        let expected = UTCTime (fromGregorian year month day) 86400
        endOfDay `shouldBe` expected
{-# LANGUAGE OverloadedStrings #-}
module EventFormaterTest where

import Test.Hspec
import EventFormater
import Data.Time
import ICalParser
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

runEventFormaterTests :: Spec
runEventFormaterTests = do
  describe "formatTechnicalEvent" $ do
    it "returns the formatted technical event string" $ do
      let event = Event
                { start = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 4 5) (timeOfDayToTime (TimeOfDay 6 15 0)))
                , end = utcTimeToPOSIXSeconds (UTCTime (fromGregorian 2023 4 5) (timeOfDayToTime (TimeOfDay 9 0 0)))
                , loc = "FPROD"
                , desc = "Projekt"
                , offsetHours = 7200
                }
      formatTechnicalEvent event `shouldBe` "08:15 - 11:00 05.04.2023\nFPROD\nProjekt"

  describe "secondsToUTCTime" $ do
    it "konvertiert Sekunden in UTCTime" $ do
      let seconds = 8 * 60 * 60 + 15 * 60
          expectedTime = UTCTime (fromGregorian 1858 11 17) (secondsToDiffTime seconds)
          actualTime = secondsToUTCTime seconds
      actualTime `shouldBe` expectedTime
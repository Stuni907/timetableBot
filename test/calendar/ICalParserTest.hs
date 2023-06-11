module ICalParserTest (runICalParserTests) where

import Test.Hspec
import ICalParser
import Parser


instance Eq Event where
  (Event start1 end1 loc1 desc1 offsetHours1) == (Event start2 end2 loc2 desc2 offsetHours2) =
    start1 == start2 && end1 == end2 && loc1 == loc2 && desc1 == desc2 && offsetHours1 == offsetHours2


runICalParserTests :: Spec
runICalParserTests = do
  describe "calendar" $
    it "parses a calendar with multiple events" $
      let input = unlines
            [ "BEGIN:VCALENDAR"            
            , "TZNAME:CET"
            , "TZOFFSETFROM:+0200"
            , "BEGIN:VEVENT"
            , "DESCRIPTION:<p></p>"
            , "DTEND;TZID=Europe/Zurich:20230515T110000"
            , "DTSTAMP;TZID=Europe/Zurich:20230609T193433"
            , "DTSTART;TZID=Europe/Zurich:20230515T081500"
            , "LOCATION:Windisch 1.313"
            , "SUMMARY:Functional Program Design (EN) 23FS 4Ibb2"
            , "END:VEVENT"
            , "BEGIN:VEVENT"
            , "DESCRIPTION:<p></p>"
            , "DTEND;TZID=Europe/Zurich:20230515T110000"
            , "DTSTAMP;TZID=Europe/Zurich:20230609T193433"
            , "DTSTART;TZID=Europe/Zurich:20230515T081500"
            , "LOCATION:Windisch 1.319"
            , "SUMMARY:Functional Program Design (EN) 23FS 4Ibb2"
            , "END:VEVENT"
            , "END:VCALENDAR"
            ]
      in (parse calendar input) `shouldBe` Just (
           [ Event { start = 1684131300
                   , end = 1684141200
                   , loc = "Windisch 1.313"
                   , desc = "Functional Program Design (EN) 23FS 4Ibb2"
                   , offsetHours = 7200
                   }
           , Event { start = 1684131300
                   , end = 1684141200
                   , loc = "Windisch 1.319"
                   , desc = "Functional Program Design (EN) 23FS 4Ibb2"
                   , offsetHours = 7200
                   }
           ],"")

  describe "locationOrOnline" $ do
    it "parses location" $
      let input = "LOCATION:London\n"
      in parse locationOrOnline input `shouldBe` Just ("London","")

    it "parses 'Online' when no location is given" $
      let input = "\n"
      in parse locationOrOnline input `shouldBe` Just ("Online","")

  describe "summary" $
    it "parses summary" $
      let input = "SUMMARY:Meeting\n"
      in parse summary input `shouldBe` Just ("Meeting","")

  describe "eventStart" $
    it "parses event start time" $
      let input = "DTSTART;TZID=Europe/Zurich:20230609T080000\n"
      in parse (eventStart 1) input `shouldBe` Just (1686294000,"")

  describe "eventEnd" $
    it "parses event end time" $
      let input = "DTEND;TZID=Europe/Zurich:20230609T090000\n"
      in parse (eventEnd 1) input `shouldBe` Just (1686297600,"")

  describe "parseCalendar" $
    it "parses a calendar and returns events" $
      let input = unlines
            [ "BEGIN:VCALENDAR"            
            , "TZNAME:CET"
            , "TZOFFSETFROM:+0200"
            , "BEGIN:VEVENT"
            , "DESCRIPTION:<p></p>"
            , "DTEND;TZID=Europe/Zurich:20230515T110000"
            , "DTSTAMP;TZID=Europe/Zurich:20230609T193433"
            , "DTSTART;TZID=Europe/Zurich:20230515T081500"
            , "LOCATION:Windisch 1.313"
            , "SUMMARY:Functional Program Design (EN) 23FS 4Ibb2"
            , "END:VEVENT"
            , "END:VCALENDAR"
            ]
      in (parseCalendar input) `shouldBe` Just (
           [ Event { start = 1684131300
                   , end = 1684141200
                   , loc = "Windisch 1.313"
                   , desc = "Functional Program Design (EN) 23FS 4Ibb2"
                   , offsetHours = 7200
                   }
           ])

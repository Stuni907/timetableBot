import Test.Hspec
import PersistDatabaseTest
import PersistDatabase
import Control.Monad (void)
import ICalParserTest (runICalParserTests)
import EventFormaterTest (runEventFormaterTests)
import CalendarTest (runCalendarTests)
import BotLogicTest (runBotLogicTests)

main :: IO ()
main = hspec $ do
  runDBTests
  runAddEntryTests
  runGetEntryTests
  runRemoveEntryTests
  runReminderTests
  describe "Cleanup" $ do
   it "should clean up resources" $ do
     cleanUpTest
  pure ()
  runICalParserTests
  runEventFormaterTests
  runCalendarTests
  runBotLogicTests
  describe "Cleanup" $ do
   it "should clean up resources" $ do
     cleanUpTest
  pure ()
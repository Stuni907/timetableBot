{-# LANGUAGE MultiParamTypeClasses #-}
module Persist (TimettableUrl,Entry,PersistSetup, PersistCreateOperation, PersistQueryOperation, PersistDeleteOperation, initDB,addEntry, getEntries, getEntry, removeEntry,getFileName, addReminder, getAllReminders, removeReminder) where

import Telegram.Bot.API (ChatId(..))
import Data.Text (Text)

type TimettableUrl = Text
type Entry = (Int, TimettableUrl)

-- | The 'PersistSetup' class defines the behavior for setting up a persistence layer for data storage.
class PersistSetup where
    -- | Returns the file name used for the data storage.
    getFileName :: String 
    -- | Initializes the persistence layer, such as creating necessary database tables or files.
    initDB :: IO ()

-- | The 'PersistCreateOperation' class defines the behavior for adding new entries to a persistence layer.
class PersistCreateOperation where
    -- | Adds a new entry to the persistence layer.
    addEntry :: Entry -> IO ()
    -- | Adds a reminder flag to an entry based on the given 'ChatId'.
    addReminder :: Int -> IO ()

-- | The 'PersistQueryOperation' class defines the behavior for querying a persistence layer for data retrieval.
class PersistQueryOperation where
    -- | Retrieves all entries from the persistence layer as a list of tuples containing an ID and a 'TimetableUrl'.
    getEntries :: IO [(Int, TimettableUrl)]
    -- | Retrieves a specific entry from the persistence layer based on the given 'ChatId'.
    getEntry :: Int -> IO (Maybe TimettableUrl)
    -- | Retrieves all entries which want to be reminded from the persistence layer as a list of tuples containing an ID and a 'TimetableUrl'.
    getAllReminders :: IO [(Int, TimettableUrl)]

-- | The 'PersistDeleteOperation' class defines the behavior for deleting entries from a persistence layer.
class PersistDeleteOperation where
    -- | Deletes an entry from the persistence layer based on the given 'ChatId'.
    removeEntry :: Int -> IO ()
    -- | Removes the reminder flag from an entry based on the given 'ChatId'.
    removeReminder :: Int -> IO ()
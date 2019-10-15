{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import           Data.Default
import           Text.ICalendar.Parser
import           Text.ICalendar.Types
import qualified RIO.Map                       as Map
import qualified RIO.Text                      as T
import qualified RIO.Text.Lazy                 as TL

import           Import                  hiding ( null )

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  app <- ask
  let options = optionsL app
      input   = optionsInFilePath options
      output  = optionsOutFilePath options
  do
    result <- liftIO $ parseICalendarFile (def :: DecodingFunctions) input
    case result of
      Left  err            -> (logError . display . T.pack) err
      Right (calendars, _) -> do
        let events = foldMap (Map.elems . vcEvents) calendars
        mapM_ (logInfo . display . formatEvent) events
    undefined

mergeEvents :: [VEvent] -> [VEvent]
mergeEvents events = undefined
  where findRec uid events' =
          let recs = filter (\e -> veUID e == uid) events'
          in


-- * Formatting

todoToken :: Text
todoToken = "APPOINTMENT"

formatEvent :: VEvent -> Text
formatEvent event =
  "* "
    <> todoToken
    <> " "
    <> maybe
         "Untitled Event"
         (\summary ->
           let summaryVal = TL.toStrict $ summaryValue summary
           in  if T.null summaryVal then "Untitled Event" else summaryVal
         )
         (veSummary event)
    <> "\n"
    <> TL.toStrict (maybe "" descriptionValue (veDescription event))
    <> "\n"
    <> ":PROPERTIES:"
    <> "\n"
    <> maybe
         ""
         (\loc ->
           let locVal = TL.toStrict $ locationValue loc
           in  if T.null locVal then "" else ":LOCATION: " <> locVal <> "\n"
         )
         (veLocation event)
    <> ":END:"
    <> "\n"

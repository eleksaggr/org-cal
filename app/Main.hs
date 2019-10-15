{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  ( main
  )
where

import           Import
import           Run
import           RIO.Process
import           Options.Applicative.Simple
import qualified Paths_org_cal

main :: IO ()
main = do
  -- (options, ()) <- simpleOptions $ (simpleVersion Paths_org_cal.version)
  --   "Header for command line arguments"
  --   "Program description, also for command line arguments"
  --   parseOptions
  --   empty
  options <- execParser (info parseOptions (fullDesc <> failureCode 1))
  lo      <- logOptionsHandle stderr (optionsVerbose options)
  pc      <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App { appLogFunc        = lf
                  , appProcessContext = pc
                  , appOptions        = options
                  }
    in  runRIO app run

parseOptions :: Parser Options
parseOptions =
  Options
    <$> switch (long "verbose" <> short 'v' <> help "Enables verbose output")
    <*> strArgument
          (metavar "input.ics" <> help "Path to the input iCalendar file")
    <*> strArgument (metavar "output.org" <> help "Path to the output Org file")

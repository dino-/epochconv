#! /usr/bin/env stack
{- stack runghc -}

{- No stack? Use this #! instead of the two lines above:
#! /usr/bin/env runhaskell
-}

{- # OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Monad ( mplus )
import Data.Time
  ( FormatTime, ParseTime, UTCTime
  , formatTime, getCurrentTime, parseTimeM, utcToLocalZonedTime
  )
import Data.Time.Format ( defaultTimeLocale )
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Text.Printf ( printf )


{-
  Some important points:

  POSIXTime is basically this: secs-since-epoch.picoseconds
  as a fractional number

  By using round on it, you can strip off the picos getting what
  we all know as epoch time. Type annotation (Int or Integer)
  may be required. Like this:

    epoch = round . utcTimeToPOSIXSeconds $ someUTCTime

  If you just need a String you can use parseTimeM with "%s"
  for format string:

    epochAsString = formatTime defaultTimeLocale "%s" someUTCTime

  But UTCTime is the low-level format that everything uses in the
  Haskell APIs

  Functions for converting between the two are in
  Data.Time.Clock.POSIX
-}


main :: IO ()
main = getArgs >>= parseInput >>= output


parseInput :: [String] -> IO (Either String UTCTime)
parseInput []         = Right  <$> getCurrentTime
parseInput ["-h"]     = Left   <$> usage
parseInput ["--help"] = Left   <$> usage
parseInput as         = pure . parseInput' $ as


usage :: IO String
usage = do
  appName <- getProgName
  now <- getCurrentTime
  pure $ unlines $
    [ "Show a given date (or the current date) in a variety of formats"
    , ""
    , "Usage: " <> appName <> " [OPTION] [DATE]"
    , ""
    , "Options:"
    , "  -e          Input is epoch (seconds since 1970-01-01)"
    , "  -m          Input is milliseconds (since 1970-01-01)"
    , "  -f          Input is a human-readable date"
    , "  -h, --help  This usage information"
    , ""
    , "If no date is given, the current date/time will be used."
    , "The -e and -f options are mostly unnecessary. -m is needed to"
    , "determine whether a long number is epoch or milliseconds."
    , ""
    , "Parsable input formats for -f:"
    ]
    <> map (\fp -> "  " <> fmt fp now) formatPatterns <>
    [ ""
    , "Output will be the date/time in a variety of formats, both localized"
    , "and UTC, as well as epoch and milliseconds."
    , ""
    , "Version 1.4  Dino Morelli <dino@ui3.info>"
    ]


parseInput' :: [String] -> Either String UTCTime
parseInput' ["-e", epochString] = strToUTCTime 1 epochString
parseInput' ["-m", milliString] = strToUTCTime 1000 milliString
parseInput' ("-f" : as)         = parseDateString . unwords $ as
parseInput' as
  -- All of the characters are not digits, probably a formatted date
  -- The tail is here to allow a negative sign for epochs prior to 0
  | not . all (flip elem "0123456789") . tail . unwords $ as
    = parseInput' $ "-f" : as
  | otherwise = strToUTCTime 1 . unwords $ as


strToUTCTime :: Double -> String -> Either String UTCTime
strToUTCTime divisor = Right . posixSecondsToUTCTime . realToFrac
  . (/ divisor) . read


parseDateString :: String -> Either String UTCTime
parseDateString s = maybe (Left $ printf "Unable to parse \"%s\"" s)
  Right $ foldl mplus Nothing $ map ($ s) parsers


rfc5322Date, iso1601Offset, iso1601Zulu :: String

-- Fri, 21 Nov 1997 10:55:06 -0500
rfc5322Date = "%a, %d %b %Y %T %z"
-- 1997-11-21T10:55:06-0500
iso1601Offset = "%FT%T%z"
-- 1997-11-21T15:55:06Z
iso1601Zulu = "%FT%TZ"


formatPatterns :: [String]
formatPatterns =
  [ "%c"
  , rfc5322Date
  , iso1601Offset
  , iso1601Zulu
  , "%F"
  ]


parsers :: ParseTime t => [String -> Maybe t]
parsers = map (parseTimeM True defaultTimeLocale) formatPatterns


output :: Either String UTCTime -> IO ()

output (Left errMsg) = do
  putStrLn errMsg
  exitFailure

output (Right ut) = do
  local <- utcToLocalZonedTime ut

  putStrLn $ "     RFC5322: " <> fmt rfc5322Date local

  printf "\n %s ISO1601: %s\n" (fmt "%Z" local) (fmt iso1601Offset local)
  putStrLn $ " UTC ISO1601: " <> fmt iso1601Zulu ut

  putStrLn $ "\n   Unix time: " <> fmt "%s" ut
  putStrLn $ "milliseconds: " <>
    (show . round . (* 1000) .  utcTimeToPOSIXSeconds $ ut)

  exitSuccess


fmt :: FormatTime t => String -> t -> String
fmt = formatTime defaultTimeLocale

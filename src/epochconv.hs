#! /usr/bin/env runhaskell

import Control.Monad ( mplus )
import Data.List ( intercalate )
import Data.Time
   ( FormatTime, ParseTime, UTCTime
   , formatTime, getCurrentTime, parseTime, utcToLocalZonedTime
   )
import Data.Time.Clock.POSIX
   ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.Locale ( defaultTimeLocale )
import Text.Printf ( printf )


{- Some important points:

   POSIXTime is basically this: secs-since-epoch.picoseconds
   as a fractional number

   By using (fromIntegral . truncate) on it, you can strip off the picos
   getting what we all know as epoch time. Or you can use parseTime "%s"

   But UTCTime is the low-level format that everything uses in the 
   Haskell APIs

   Functions for converting between the two are in
   Data.Time.Clock.POSIX
-}


main :: IO ()
main = getArgs >>= parseInput >>= output


parseInput :: [String] -> IO (Either String UTCTime)
parseInput []         = Right `fmap` getCurrentTime
parseInput ["-h"]     = Left `fmap` usage
parseInput ["--help"] = Left `fmap` usage
parseInput as         = return . parseInput' $ as


usage :: IO String
usage = do
   appName <- getProgName
   now <- getCurrentTime
   return $ unlines $
      [ "Show a given date (or the current date) in a variety of formats"
      , ""
      , "Usage: " ++ appName ++ " [OPTION] [DATE]"
      , ""
      , "Options:"
      , "  -e          Input is epoch (seconds since 1970-01-01)"
      , "  -m          Input is milliseconds (since 1970-01-01)"
      , "  -f          Input is a human-readable date"
      , "  -h, --help  This usage information"
      , ""
      , "If no date is given, the current date/time will be used."
      , "The -e and -f options are mostly unnecessary. -m is needed to"
      , "disambiguate between epoch and milliseconds."
      , ""                                                 
      , "Parsable input formats for -f:"
      ]
      ++ (map (\fp -> "   " ++ fmt fp now) formatPatterns) ++
      [ ""                                                 
      , "Output will be the date/time in a variety of formats, both localized"                                                 
      , "and UTC, as well as epoch and milliseconds."
      , ""                                                 
      , "Dino Morelli <dino@ui3.info>"                     
      ]


parseInput' :: [String] -> Either String UTCTime
parseInput' ("-e" : epochString : []) = strToUTCTime 1 epochString
parseInput' ("-m" : milliString : []) = strToUTCTime 1000 milliString
parseInput' ("-f" : as)               = parseDateString $ joinArgs as
parseInput' as
   | any (not . (flip elem) "-0123456789")
      $ joinArgs as = parseInput' $ "-f" : as
   | otherwise = strToUTCTime 1 (joinArgs as)


strToUTCTime :: Double -> String -> Either String UTCTime
strToUTCTime divisor = Right . posixSecondsToUTCTime . realToFrac
   . (/ divisor) . read


joinArgs :: [String] -> String
joinArgs = intercalate " "


parseDateString :: String -> Either String UTCTime
parseDateString s = maybe (Left $ printf "Unable to parse \"%s\"" s)
   (Right . id) $ foldl mplus Nothing $ map ($ s) parsers


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
parsers = map (parseTime defaultTimeLocale) formatPatterns


output :: Either String UTCTime -> IO ()

output (Left errMsg) = do
   putStrLn errMsg
   exitFailure

output (Right ut) = do
   local <- utcToLocalZonedTime ut

   putStrLn $ "     RFC5322: " ++ fmt rfc5322Date local

   printf "\n %s ISO1601: %s\n" (fmt "%Z" local) (fmt iso1601Offset local)
   putStrLn $ " UTC ISO1601: " ++ fmt iso1601Zulu ut

   putStrLn $ "\n   Unix time: " ++ fmt "%s" ut
   putStrLn $ "milliseconds: " ++ (show . fromIntegral . truncate
      . (* 1000) . realToFrac . utcTimeToPOSIXSeconds $ ut)

   exitSuccess


fmt :: FormatTime t => String -> t -> String
fmt = formatTime defaultTimeLocale

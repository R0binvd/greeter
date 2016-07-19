import Data.List (isInfixOf)
import Data.Time.Clock (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Fixed (divMod')
import Data.Time.LocalTime (getCurrentTimeZone, ZonedTime, TimeZone, timeZoneOffsetString, zonedTimeToUTC)

main :: IO ()
main = do
  log <- readFile "/var/log/pacman.log"
  currentTime <- getCurrentTime
  timeZone <- getCurrentTimeZone
  
  putStrLn $ format $ diffTime currentTime $ getLast log timeZone

--Gets the last full system upgrade and parses it into a Maybe ZonedTime
getLast :: String -> TimeZone -> Maybe ZonedTime
getLast log timeZone =
  parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M %z" (format ++ " " ++ timeoffset) :: Maybe ZonedTime
  where  
    format = (drop 1 $ take 17 $ last $ filter (isInfixOf "starting full system upgrade") $ lines log)
    timeoffset = timeZoneOffsetString timeZone

--Returns the diff of the last upgrade and the currentime
diffTime :: UTCTime -> Maybe ZonedTime ->Maybe NominalDiffTime
diffTime _ Nothing = Nothing :: Maybe NominalDiffTime
diffTime y (Just x)  = Just $ diffUTCTime y $ zonedTimeToUTC x

--formats the diff time in days and hours and adds some text
format :: RealFrac a => Maybe a -> String
format Nothing = "Error"
format (Just secs)
  | days == 0 && hours < 1 = " < 1 hours since last pacman -Syu"
  | otherwise = " " ++ show days ++ " days, " ++ show hours ++ " hours since last pacman -Syu"
  where days =  fst x
        hours = floor $ snd x
        x = divMod' (secs/3600) 24  


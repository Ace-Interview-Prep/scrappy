
module Main (main) where

import Test.WebDriver
import Test.WebDriver.Session
import Test.WebDriver.Common.Profile
import Control.Monad.IO.Class

import Scrappy.Links
import Scrappy.Requests

import Control.Monad (void)
import Data.Char (toLower)



main :: IO ()
main = void $ testWebdriver

lower = fmap toLower

testWebdriver = do
  wdSesh <- runSession defaultConfig $ getSession  --mkWDSesh
  getHtmlST wdSesh $ Link "http://localhost:8020/main/home"
  

mkWDSesh :: IO WDSession 
mkWDSesh = do
  wdConfig <- myConfig
  liftIO $ print "made WDConfig for local system"
  liftIO $ runSession wdConfig $ getSession

-- :: IO Browser
myfirefox = do

  liftIO $ print "getting profile"
  -- x <- prepareLoadedProfile_ "/home/lazylambda/.mozilla/firefox/wrfni7ol.default/"
  print "INPUT NEEDED: Who's computer is this?"
  print "Options: Sandra | GalenNixOS | GalenMacBootcamp" 
  someName <- getLine
  print someName
  let
    -- we could determine system by simply asking the user their name or
    -- there may exist some library that checks this for us?
    determineSystem = undefined
    system = determineSystem
    profilePath = (\nameInHead -> case lower nameInHead of
                      "galenmacbootcamp" -> pure "C:/Users/galen/AppData/Roaming/Mozilla/Firefox/Profiles/qsd0h2br.default"
                      "galennixos" -> pure "/home/lazylambda/.mozilla/firefox/freeScrape"
                      "sandra" -> error "you need to ask for this from Lauren" -- error is like undefined
                      _ -> print (lower nameInHead) >> undefined
                  ) someName
  
  path <- profilePath
  -- fileExists <- doesPathExist $ path <> "/lock"
  -- when fileExists $ removeFile (path <> "/lock")
  print "got here"
  print "you may need to delete a 'lock' file in your profile dir if the next step fails"
  print "If so, execute this: rm /path/to/profile/lock"
  print $ "profile path: " <> path
  x <- prepareLoadedProfile_ path
  liftIO $ print "profile found"
  return $ firefox { ffProfile = Just x }


  
myCaps = do
  x <- myfirefox
  return $ defaultCaps { browser = x }
myConfig :: IO WDConfig
myConfig = do
  x <- myCaps
  return $ defaultConfig { wdCapabilities = x }

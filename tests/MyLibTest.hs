
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

-- this is really the testing for Ace server atm

import Test.WebDriver as WD --hiding (click)
import Test.WebDriver.Session
import Test.WebDriver.Class
import Test.WebDriver.Common.Profile
import Test.WebDriver.Commands.Wait
import Control.Monad.IO.Class

import Scrappy.Links
import Scrappy.Requests
import Scrappy.Scrape
import Scrappy.Elem
import Scrappy.Find
import Text.Parsec (string, parse)


import Control.Monad (void, when, forM_)
import Data.Char (toLower)

import System.Process
import System.Which
import Control.Concurrent (threadDelay)

import qualified Control.Exception.Lifted as Lifted
import qualified Control.Monad.Trans.Control as Lifted
import Control.Monad.Trans.Maybe
import Control.Monad.Catch
import Control.Exception as CE

import Data.Either 
import qualified Data.Text as T


main :: IO ()
main = void $ do
  
  startAce
  --callCommand "which chromedriver"
--  print chromedriver
  startSelenium
--  threadDelay $ 3 * 1000000
  testWebdriver


openPageD p = openPage p >> delay 3  

geckodriver = $(staticWhich "geckodriver")
--chromedriver = $(staticWhich "chromedriver")
chromeBinary' = $(staticWhich "google-chrome-stable")
seleniumServer = $(staticWhich "selenium-server")
java = $(staticWhich "java")

tryToss :: forall m. MonadIO m => IO () -> m ()
tryToss m = do
  e :: (Either SomeException ()) <- liftIO $ CE.try $ m
  when (isLeft e) $ liftIO $ print e
  pure ()

tryTossLifted :: forall b m. (Lifted.MonadBaseControl IO m, MonadIO m) => m () -> m ()
tryTossLifted m = do
  e :: (Either SomeException ()) <- Lifted.try $ m
  pure ()

startAce = do
  tryToss $ callCommand "pkill backend"
  callCommand "cd /home/lazylambda/code/Ace/Ace/test-app/; rm -rf dbReal ; ./backend --port 8020 &"
  threadDelay $ 5 * one_second

startSelenium = do
  tryToss $ callCommand "pkill firefox"
  --callCommand $ "export PATH=$PATH:" <> chromedriver <> "/bin"
  --callCommand $ java <> " -jar " <> seleniumServer <> "/selenium-server-*.jar"
  --callCommand $ java <> " -jar " <> seleniumServer -- <> "/selenium-server-*.jar"
  callCommand $ seleniumServer <> " &"

lower :: String -> String
lower = fmap toLower

click' :: (MonadThrow m, MonadIO m, SessionState s) => s -> Clickable -> m (String, s) 
click' = Scrappy.Requests.click "unexpected-file" 

-- | even though this is unsafe this should not leave ace local host
mkClickableUnsafe :: ElementRep e => e a -> Maybe Clickable
mkClickableUnsafe = mkClickable True (Link "http://localhost:8000/main/auth/login")


--newtype ScrappyT m a = ScrappyT { runScrappyT :: MaybeT m a } 
printT :: (MonadIO m, Show a) => a -> m ()
printT = liftIO . print

failedText = "<button class=\"flex w-full rounded-lg bg-[#00B9DA] font-semibold justify-center text-white font-[Sarabun] px-8 sm:px-12 py-5 mt-8 sm:mt-24 mx-auto\">Log in</button>"

-- ThisIsForTestingAce123!
-- Proton: user='testing-ace2', pass= 'ThisIsForTestingAce123!'



email = "testing-ace2@proton.me"
password = "ThisIsForTestingAce123!"
orgName = "FakeOrg"
userEmail1 = "testing-ace2-user1@proton.me"
userEmail2 = "testing-ace2-user2@proton.me"
userEmail3 = "testing-ace2-user3@proton.me"

type XPathB = (T.Text, (T.Text, T.Text))

findElemX :: WebDriver wd => T.Text -> wd Element
findElemX = findElem . ByXPath 

-- | Good typing for most xpath expressions
findElemX' :: WebDriver wd => XPathB -> wd Element
findElemX' = findElem . ByXPath . toXPath

findElemsX :: WebDriver wd => T.Text -> wd [Element]
findElemsX = findElems . ByXPath 

findElemsX' :: WebDriver wd => XPathB -> wd [Element]
findElemsX' = findElems . ByXPath . toXPath 

toXPath :: (T.Text, (T.Text,T.Text)) -> T.Text
toXPath (hTag, (attrName, attrVal)) = mconcat [ "//", hTag,  "[@", attrName,  "='" , attrVal, "']"]


delay :: MonadIO m => Int -> m ()
delay = liftIO . threadDelay . (*one_second) 

type Microseconds = Int
one_second :: Microseconds 
one_second = 1000000

doCompanySignup :: WD ()
doCompanySignup = do
  openPage "http://localhost:8020/main/admin/new-company-signup?code=edcbcc2a105729f1679c47f57a14df44d9a54bdf8b9962c35c5108c698b8228a7cacc696fd5f2be05b208cf9362d7a1c57da0bbfc2e9403093ea02a8b63f365f"
  email1 <- findElem (ByXPath "//input[@placeholder='Enter your email']")
  email2 <- findElem (ByXPath "//input[@placeholder='Re-enter your email']")
  orgNameEl <- findElem (ByXPath "//input[@placeholder='Organization Name']")
  sendKeys email email1
  sendKeys email email2
  sendKeys orgName orgNameEl
  findElem (ByXPath "//input[@type='checkbox']") >>= WD.click
  let bXpath = "//button[@class='focus:outline-none w-full p-4 mt-16 shadow-button bg-[#00B9DA]  font-[Sarabun] font-bold text-white text-body text-center rounded-xl  hover:bg-primary-rich active:bg-primary-desaturated  focus:ring-4 ring-primary ring-opacity-50  transition-all duration-300 ease-in-out  transform hover:scale-105 active:scale-95  hover:shadow-md active:shadow-lg']"
  findElemX bXpath >>= WD.click
  liftIO $ threadDelay $ 10 * one_second


loginAce :: T.Text -> T.Text -> WD ()
loginAce thisEmail thisPass = do
  openPage "http://localhost:8020/main/auth/login"
  delay 2
  findElemX' ("input", ("placeholder", "Enter your email")) >>= sendKeys thisEmail
  findElemX' ("input", ("placeholder", "Enter your password")) >>= sendKeys thisPass
  findElemX' ("button", ("class", "flex w-full rounded-lg bg-[#00B9DA] font-semibold justify-center text-white font-[Sarabun] px-8 sm:px-12 py-5 mt-8 sm:mt-24 mx-auto"))
    >>= WD.click
  

signoutProton :: WD ()
signoutProton = do
  findElemX' ("button", ("data-testid", "heading:userdropdown")) >>= WD.click
  findElemX' ("button", ("data-testid", "userdropdown:button:logout")) >>= WD.click 

clickLinkInEmail :: T.Text -> WD ()
clickLinkInEmail email = do
  let loginURL = "https://account.proton.me/login"
  openPage loginURL
  delay 3
  getCurrentURL >>= \case
    loginURL -> pure ()
    _ -> signoutProton >> delay 3
  liftIO $ threadDelay $ 5 * one_second
  findElemX "//input[@id='username']" >>= sendKeys email
  findElemX "//input[@id='password']" >>= sendKeys password
  findElemX "//button[@class='button w-full button-large button-solid-norm mt-6']" >>= WD.click
  delay 10
  -- as you can see i said fuck it; this forces the last email to be the one getting opened 
  forM_ [1..3] $ \_ -> do 
    findElemsX "//div[@class='item-container-wrapper relative border-bottom border-weak']" >>= WD.click . head
    delay 1 
    back
  delay 1 
  findElemsX "//div[@class='item-container-wrapper relative border-bottom border-weak']" >>= WD.click . head
  delay 5
  -- printT . length =<< findElemsX "//iframe[@title='Email content']"
  -- printT . length =<< findElemsX' ("article", ("class", "message-container mx-2 my-4 relative"))
  -- printT . length =<< findElemsX' ("article", ("data-shortcut-target", "message-container"))
  findElemsX' ("article", ("data-shortcut-target", "message-container")) >>= WD.click . last
  -- printT . length =<< findElemsX' ("iframe", ("title", "Email content"))
  findElemsX' ("iframe", ("title", "Email content")) >>= focusFrame . WithElement . head
  getSource >>= \html -> do
    case flip scrapeFirst' (T.unpack html) $ hrefParser of
    --case flip scrape (T.unpack html) $ hrefParser of 
      Nothing -> error "no link found"
      Just l -> do
        printT l
        -- streamEdit: needed due to bug with Obelisk link only affecting local builds
        openPage l
        -- tryTossLifted $ openPage l
        -- url8020 <- getCurrentURL
        -- openPage $ streamEdit (string ":8020") (const ":8000") url8020

setPasswordsAce :: WD ()
setPasswordsAce tPassword = do
  findElemX' ("input", ("placeholder", "Enter your new password")) >>= sendKeys tPassword
  findElemX' ("input", ("placeholder", "Re-enter your new password")) >>= sendKeys tPassword
  findElemX' ("button", ("class", "focus:outline-none w-full p-4 mt-16 shadow-button bg-primary  font-facit font-bold text-white text-body text-center rounded  hover:bg-primary-rich active:bg-primary-desaturated  focus:ring-4 ring-primary ring-opacity-50"))
    >>= WD.click

  delay 4
  pure ()



addUsers :: WD ()
addUsers = do
  findElemX' ("input", ("placeholder","example1@gmail.com, example2@gmail.com, example3@gmail.com, ..."))
    >>= sendKeys (userEmail1 <> "," <> userEmail2 )
  findElemX' ("button", ("class", "focus:outline-none w-full p-4 mt-16 shadow-button bg-[#00B9DA]  font-[Sarabun] font-bold text-white text-body text-center rounded-xl  hover:bg-primary-rich active:bg-primary-desaturated  focus:ring-4 ring-primary ring-opacity-50  transition-all duration-300 ease-in-out  transform hover:scale-105 active:scale-95  hover:shadow-md active:shadow-lg"))
    >>= WD.click

-- | Note: userX is AcctID = x
-- | TODO: users pick different options (for now theres almost no possibility of this causing issues)


doOnboardingForm :: T.Text -> WD ()
doOnboardingForm name = do
  findElemX' ("input", ("class", "w-full px-4 py-3 border border-gray-300 rounded-lg focus:outline-none focus:border-[#00B9DA] font-[Sarabun] text-lg mb-5"))
    >>= sendKeys name 
  findElemX' ("button", ("class", "rounded-lg bg-[#00B9DA] font-semibold text-white font-[Sarabun] px-8 py-5 mt-8 ml-10 md:px-48 md:py-5 md:mt-10 mr-30 pr-5"))
    >>= WD.click

-- From homepage 
gotoMockInterview :: WD ()
gotoMockInterview = do
  findElemsX' ("a", ("class", "flex justify-center w-1/2 block mt-10 mb-4 bg-[#2E3A59] text-white px-6 py-2 rounded-xl hover:bg-[#00B9DA] font-bold font-sans transition-colors duration-300"))
    >>= \_:e:_:[] -> WD.click e
  delay 3
  findElemX' ("a",("class","text-center float-right text-white bg-[#00B9DA] font-bold font-[Sarabun] rounded-[12px] py-6 px-40")) >>= WD.click
  pure ()

doMockInterview :: WD ()
doMockInterview = do
  pure () 
    
-- | TODO: use postgres libs to test more cases of input
-- | Note: the only reason we need 3 users is to test stuff like leaderboard and the admin view ... in terms of
-- | brute-forcing variants we can more directly call the systems

-- | TestType: FE test ; reps normal UX can and cant do's , BE test ; brute-forcing values  ; where we may need to undo actions that
-- | on the FE users would be unable to do n > 1 times 


-- 10 videos over 2 reports
-- use offensive name
user1 :: WD () 
user1 = do
  let email = userEmail1
  -- NOTE: this input is as offensive as it gets, but we need to ensure the worst is definitively stopped
  offensiveName <- liftIO $ readFile "offensive-input" 
  clickLinkInEmail email
  setPasswordsAce password
  doOnboardingForm $ T.pack offensiveName
  doMockInterview 
  pure () 

-- just 1 video
-- use ok but questionable name: Nigerian-Mamba
user2 = do
  let email = userEmail2
  clickLinkInEmail email
  setPasswordsAce password
  doOnboardingForm "Nigerian-Mamba"
  pure ()

-- just onboard
-- Use basic name 
user3 = do
  let email = userEmail3
  clickLinkInEmail email
  setPasswordsAce password
  doOnboardingForm "Chris the Guy"
  -- do report 1
  -- do exercises
  -- do report 2
  -- 
  pure ()
  
  
testWebdriver :: IO () 
testWebdriver = do
  tryToss $ do
    runSession defaultConfig $ do
      openPage "https://www.youtube.com/watch?v=8zd8qO53m_Y&list=PLOj29xo-aR4T8gmphpzyrN-MwgJJorUfV"
      --findElemX' ("button",("class", "ytp-large-play-button ytp-button")) >>= WD.click
      delay 3
      findElemX' ("a", ("class","yt-simple-endpoint style-scope ytd-playlist-panel-video-renderer")) >>= WD.click
    runSession defaultConfig $ do
      delay 3
      doCompanySignup
      clickLinkInEmail email 
      setPasswordsAce password
      --  loginAce email "Warhawks58!" -- temp : proton mail exhausted
      addUsers
      user1
      -- user2
      -- user3
      -- loginAce email "Warhawks58!" -- temp : proton mail exhausted
      -- checkAdminPage
      -- checkLeaderboard 
    
    
  delay 6
  tryToss $ callCommand "pkill backend"


--http://localhost:8000/reset/xieafvJ1M8M84lccl5Yv48QUBGfVxgIfl0wvWoclZJ+U3iVKjGM88eDpoTaMljlaPApWIIMNmNT9mwm68k1U8bRLAFf4O1E5rrYyYBpvws+mjFs4NPnZbsgf%2Fd0fe9Bbm%2FH7wVOFau4eK+kXgPYQQOKAGLTzNtwuJjxow42%2FCYV4dDQa3mZ5IKqblaHxtqM8WMoF

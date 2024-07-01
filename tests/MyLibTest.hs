{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

-- this is really the testing for Ace server atm


import Data.Maybe
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


import Control.Monad (void, when, forM_, replicateM_)
import Data.Char (toLower)

import System.Process
import System.Which
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)

import Control.Applicative (some)
import qualified Control.Exception.Lifted as Lifted
import qualified Control.Monad.Trans.Control as Lifted
import Control.Monad.Trans.Maybe
import Control.Monad.Catch
import Control.Exception as CE

import Data.Either 
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List

import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header
import Network.HTTP.Client.Internal hiding (Proxy)

import Data.Aeson as Aeson
import Data.Bifunctor


import Text.Read (readEither)
import Data.Aeson -- (FromJSON, ToJSON, Value(..), parseJSON, toJSON, (.:), object, (.=))
import qualified Data.Map as Map 
import qualified Data.Text.Lazy as LT 
import qualified Data.Text as T
import qualified  Data.ByteString.Lazy  as LBS --(ByteString, toStrict)
import qualified Data.ByteString as BS


import Text.Parsec 

import qualified Data.Map as Map
import GHC.Generics



import Scrappy.Elem.ITextElemParser

-- | Imagine how powerful this would be if we could plug into 


-- | TODO: use postgres libs to test more cases of input
-- | Note: the only reason we need 3 users is to test stuff like leaderboard and the admin view ... in terms of
-- | brute-forcing variants we can more directly call the systems

-- | TestType: FE test ; reps normal UX can and cant do's , BE test ; brute-forcing values  ; where we may need to undo actions that
-- | on the FE users would be unable to do n > 1 times 



-- | Most Efficient Test
-- | 
-- admin actions
-- submit on all N user accounts
-- run all usertype=Self tests
-- run all usertype=Admin tests
-- run all API + email tests
-- review all user questions


main = scrapeSite ""


-- <html <a>...about...<a>about us</a>   </a> 


-- <a>About us</a>

-- scrape html getPlainText

type HTML = String 
getAboutUsLink :: HTML -> [Maybe URL]
getAboutUsLink html  = getHrefs $ (flip scrape) html $ do
  --el "a" [] `contains'` ((string "about") <|> string "About")
  elemParser (Just ["a"]) (Just check) []  



--check = oneOf ['A','a'] >> string "bout"

{- 
ignoreCase :: String -> [String] -}


buildOpts :: Stream s m Char => [(Int,String)] -> ParsecT s u m (Int, String)
buildOpts [] = parserZero
buildOpts ((rank,x):elemsAllow) = (fmap (rank,) $ try (string x)) <|> (buildOpts elemsAllow)
  where
    try' = Text.Parsec.try 



aboutUs :: ParsecT s u m (Int, String) 
aboutUs = buildOpts $ zip [1..] 
  [ "about us"
  , "about"
  , "company story"
  , "our mission"
  , "our story"
  , "who we are"
  , "about the company"
  , "meet the team"
  , "company history"
  , "what we do"
  , "our values"
  , "company overview"
  , "corporate profile"
  , "about our team"
  , "company background"
  , "company info"
  , "about our business"
  , "about our firm"
  , "about our organization"
  , "who we are and what we do"
  , "about our story"
  , "about our journey"
  , "about our founders"
  ]
  


{- 
Elem' {_el = "a", _attrs = fromList [("class","item-submenu w-inline-block"),("href","/company"),("id","w-node-_18b38e1e-57fa-461b-b9af-7cb927f59f44-2dfcabeb")], innerMatches = [(2,"About Us"), (3,"About Wildlife")], innerHtmlFull = "<img src=\"https://assets-global.website-files.com/63358611d4c8ab8cb0b5a30e/641ac460a2ef747ec0168432_Icon-suitcase.svg\" loading=\"lazy\" alt=\"\" class=\"icon-item-list-submenu\"/><div class=\"item-submenu-wrapper-text\"><h4 class=\"body-s semibold\">\"About Us\"</h4><p class=\"body-xs opacity-50\">Your dealmaking partner</p></div>"}

[(2,"About Us"), (3,"About Wildlife")]

pickHighestRank :: [(Int, String)] -> String
 -}

getHrefs :: Maybe [Elem' String] -> [Maybe String]
getHrefs (Just x) = fmap (Map.lookup "href" . _attrs) x
  -- let as = _attrs x
  -- in Map.lookup "href" as

--elemParser (Just ["a"]) (Just $ string "about") [] 

main :: IO ()
main = do 
  let csvFilePath = undefined :: String 
  csvInput <- parseCSVFromFile csvFilePath

  let headers = head csvInput
  let Just urlColumn = findIndex (== "Website") headers

  {- let headersAboutUrlFailure = "" "AboutUrl Result" : headers
  let headersGPTOverloadFailure = "AboutUrl Result" : headers
  -}
  

  eithRows <- handleRows urlColumn csvInput


  let (failed, successful) = partitionEithers eithRows 
  let (aboutPageNotFound, gptOverloaded) = partition ((==) ABOUT_PAGE_NOT_FOUND . fst) failed

  let
    -- Transform to row  
    emptyPersonalizedLine = ""
    failedFindLink = fmap (\(ABOUT_PAGE_NOT_FOUND, row) -> emptyPersonalizedLine : (show ABOUT_PAGE_NOT_FOUND) : row) aboutPageNotFound
    failedGPT = fmap (\((TOKEN_LIMIT_REACHED url), row) -> emptyPersonalizedLine : url : row  ) gptOverloaded

  let headers = "PersonalizedLine" : "AboutUrl Result" : headers  
  let toCSV = printCSV . (headers :)
  writeFile "failedFindLink.csv" $ toCSV failedFindLink
  writeFile "failedGPT.csv" $ toCSV failedGPT
  writeFile "successful.csv" $ toCSV successful




handleRows :: Int -> [[Field]] -> IO [Either (AutomationError, [Field]) [Field]]
handleRows 
handleRows urlColumn (r:rows) = do
  handleRow urlColumn r
  handleRows urlColumn rows



--sortBy :: (a -> a -> Ordering) -> [a] -> [a]
toRankedLink :: Elem' a -> Maybe (Int, URL)
toRankedLink element = 
  let 
    sortedMatches = sortBy fst $ innerMatches element
    maybeHref = Map.lookup "href" $ _attrs element
  case maybeHref of 
    Nothing -> Nothing
    Just hrefLink -> case sortedMatches of 
      [] -> Nothing 
      ((rank,_):_) ->  Just (rank, hrefLink)

      -- rankList
      -- (first:rest)
      -- (first:_)
      -- ((rank,_):_)

--fmap toRankedLink (elems :: [Elem' String])   :: [Maybe (Int,URL)]


--catMaybes :: [Maybe a] -> [a]


-- data Elem' a = Elem' { _el :: Elem -- change to Elem?
--                      , _attrs :: Map String String --Attrs needs to become map
--                      , innerMatches :: [a] --will be "" if not specified
--                      , innerHtmlFull :: String
--                      } deriving Show


data AutomationError = TOKEN_LIMIT_REACHED URL | ABOUT_PAGE_NOT_FOUND


max_GPT_INPUT_LENGTH = 1000




pure :: a -> m a
pure :: a -> IO a 

do 
  (x :: Int) <- (pure 1 :: IO Int)




type GPTPersonalizedLine = T.Text
type URL = String 
handleRow :: Int -> [Field] -> IO (Either (AutomationError, [Field]) [Field]) -- GPTPersonalizedLine
handleRow url_idx row = do
  let url = row !! url_idx
  print url 
  html <- getHtml' url

  case flip scrape (toLower html) $ elemParser (Just ["a"]) aboutUs [] of 
    Nothing -> pure $ Left $ (ABOUT_PAGE_NOT_FOUND, row)
    Just elems -> sortBy fst $ catMaybes $ fmap toRankedLink elems of 
      [] -> pure $ Left $ (ABOUT_PAGE_NOT_FOUND, row)
      ((_, hrefLink):_) -> do
        --pure hrefLink
        print hrefLink
        aboutUsHtml <- getHtml' hrefLink 
        case flip scrape html $ el "body" [] `contains'`paragraph of 
          Nothing -> pure $ Left $ (ABOUT_PAGE_NOT_FOUND, row)
          Just paragraphsAbout -> case flip scrape html $ el "body" [] `contains'`paragraph of 
            Nothing -> pure $ Left $ (ABOUT_PAGE_NOT_FOUND, row) -- Not totally true
            Just paragraphsHome -> do
              let   
                Paragraphs textTotal = mconcat paragraphsAbout <> mconcat paragraphsHome 
              case length textTotal > max_GPT_INPUT_LENGTH of 
                True -> pure $ Left $ (TOKEN_LIMIT_REACHED hrefLink, row)
                False -> do 
                  k <- readFile "gptAPIKey" 
                  mgr <- newManager tlsManagerSettings
                  gptResponse <- askGPT (APIKey k) mgr $ 
                    [ cwr System  "You are reaching out to a business owner who you don't know. You talk in professional but concise language."
                    , cwr User $ "Write a personalized compliment about a business. The compliment should be one sentence long, \
                    \using concise language that an eighth grader could understand. Do not start with a greeting. \
                    \Write the compliment based on this description: " <> textTotal            
                    ]
                  case gptResponse of 
                    Left e -> error $ show e 
                    Right personalizedLine -> pure $ Right $ personalizedLine : hrefLink : row
        


{- Will says:completion = client.chat.completions.create(
    model="gpt-4",
    messages=[
      {"role": "system", "content": "You are reaching out to a business owner who you don't know. You talk in professional but concise language."},
      {"role": "user", "content": "Write a personalized compliment about a business. The compliment should be one sentence long, using concise language that an eighth grader could understand. Do not start with a greeting. Write the compliment based on this description: {description}"}
    ]
  ) 
 -}  



      -- case catMaybes $ fmap (Map.lookup "href" . _attrs) . f $ elems of -- :: [URL] 
      -- [] -> print "No About us " >> pure Nothing
      -- links ->  
  
  
  -- --let field = fromMaybe "" maybeAboutUsLink 


  -- mapM_ print . f . fromMaybe []  $ flip scrape (toLower html) $ do
  --   elemParser (Just ["a"]) (Just $ string "About Us") []  
  -- --let baseUrl = getBaseUrl url 

  -- let aboutUsLink :: [Maybe URL] = getAboutUsLink html
  -- let Just plainText = (flip scrape) html onlyPlainText

  -- mapM_ print . fromMaybe [] $ flip scrape html $ el "div" [] `contains'`paragraph


  -- el "body" `contains'` (onlyPlainText )
  
  -- --mapM_ print aboutUsLink-- , plainText)
  -- pure ""



data TokenLimitedContent = TokenLimitedContent (Maybe Int) [ContentWithRole]
type TokenLimit = Maybe Int
-- | TODO: change to gptPrim
askGPT :: MonadIO m => APIKey 'OpenAI -> Manager -> TokenLimit -> [ContentWithRole] -> m (Either T.Text T.Text)
askGPT apiKey mgr maxTokens contents = liftIO $ do
  --mgr <- newManager tlsManagerSettings
  print "askGPT"
  let url = "https://api.openai.com/v1/chat/completions"
  req <- parseRequest url
  let headers = [ (hAuthorization, "Bearer " <> (T.unpack . T.strip . unAPIKey $ apiKey))
                , (hContentType, "application/json")
                ]
  let gptJsonType = GPTResponseFormat GPT_JSON
  --let max_tokens = Just 1
  let promptLen = maybe [] (\len -> [cwr System $ "Please limit response to " <> (T.pack . show $ 50) <> " tokens"]) maxTokens
  -- We add 50 to limit because as the request gets larger GPT is worse at knowing when to stop
  -- This should not affect shorter responses
  let prompt = GPTRequestBody gptModel ((+ 50) <$> maxTokens) $ promptLen <> contents
  let req' = req { requestHeaders = (fmap . fmap) (T.encodeUtf8 . T.pack) headers
                 , method = "POST"
                 , requestBody = RequestBodyLBS $ Aeson.encode prompt --txt
                 --, responseTimeout = ResponseTimeoutMicro 1000000000
                 }
  liftIO $ print $ Aeson.encode prompt
  --resBody <- fmap responseBody $ httpLbs req' mgr
  (CE.try $ fmap responseBody $ httpLbs req' mgr) >>= \case
    Left (e :: HttpException) -> pure $ Left $ tshow e
    Right resBody -> case eitherDecode resBody :: Either String PromptResponse of
      Left e -> case eitherDecode resBody :: Either String ErrorResponseOpenAI of
        Left ee -> pure . Left . T.pack $ e
        Right (ErrorResponseOpenAI (ErrorOpenAI msg _ _ _)) -> do
        --emailGalen $ "Error with OpenAI: " <> m
          liftIO $ print $ "Error with OpenAI: " <> msg
          pure $ Left "Unknown AI Error"
        --pure . Left $ T.pack e <> ":" <> (T.decodeUtf8 . LBS.toStrict $ resBody)
      Right r -> case choices r of
        [] -> pure $ Left "No choices"
        chcs -> pure . Right . content . message . head $ chcs



  -- case aboutUsLink of
  --   Nothing -> pure ""
  --   Just aboutLink -> do
  --     aboutUsHtml <- getHtml' $ baseUrl <> aboutLink
  --     let Just aboutPlainText = scrape aboutUsHtml onlyPlainText

  --     let totalTxt = plainText <> aboutPlainText 

  --     fp <- getFilename csv
  --     totalTxt' <- readFile fp 

  --     let prompt = [ cwr System "Respond using only emojis"-- ContentWithRole
  --                  , cwr User "how are you today?"
  --                  ]
  --     k <- readFile "gptAPIKey"
  --     let
  --       key :: APIKey 'OpenAI
  --       key = APIKey k
  --     pure "" 
      -- mgr <- mkManager 
      --gptResponse <- askGPT key (Just 100) prompt
      -- case gptResponse of
      --   Left _ -> undefined
      --   Right txt -> txt

-- . | ( $ * #@ @( @( @)_!  } Token



-- f :: Maybe [Link]
-- f = scrape html href 






















-- main :: IO ()
-- main = void $ do
--   --startAce False 
--   startSelenium
--   testWebdriver False False 

-- startAce :: Bool -> IO () 
-- startAce shouldInterpret = do
--   tryToss $ callCommand "fuser -k 8020/tcp"
--   case shouldInterpret of
--     False -> do
--       callCommand "cd /home/lazylambda/code/Ace/Ace/test-app/ ; ./backend --port 8020 &"
--       delay 5
--     True -> do
--       error "not yet implemented: must force working browser"
--       callCommand "cd /home/lazylambda/code/Ace/Ace/test-app-interpret/; ob run &"
--       delay 10

-- -- | TODO: shouldInterpret=True may not have correct DB
-- testWebdriver :: Bool -> Bool -> IO () 
-- testWebdriver shouldInterpret shouldRunAccountSetup = do
--   case shouldRunAccountSetup of
--     True -> do
--       useNewDb
--       createAllAccounts
--     False -> do
--       useSetupDb
--   liftIO $ startAce shouldInterpret

--   tryToss $ do
--     -- runSession defaultConfig $ do
--     --   openPage "https://www.youtube.com/watch?v=8zd8qO53m_Y&list=PLOj29xo-aR4T8gmphpzyrN-MwgJJorUfV"
--     --   --findElemX' ("button",("class", "ytp-large-play-button ytp-button")) >>= WD.click
--     --   delay 3
--     --   findElemX' ("a", ("class","yt-simple-endpoint style-scope ytd-playlist-panel-video-renderer")) >>= WD.click
--     runSession defaultConfig $ do
--       testPlan 
--   delay 6
--   tryToss $ callCommand "pkill backend"


-- testPlan :: WD () 
-- testPlan = do
--   withUser userEmail1 $ do
--     startNewMockInterview 
--     doSomeMockInterviewQuestions 6
--   -- delay 3
--   -- doCompanySignup
--   -- clickLinkInEmail email 
--   -- setPasswordsAce password
--   -- --  loginAce email "Warhawks58!" 
--   -- addUsers
--   -- user1
--   -- user2
--   -- user3
--   -- loginAce email "Warhawks58!" 
--   -- checkAdminPage
--   -- check each users reports 
--   -- checkLeaderboard 
--   -- recoverPassword_User1
--   -- test no internet
--   -- doExercises
--   -- checkEmailsSentToAce
--   --   -- interviewer metrics
--   --   -- submissions, activity, etc
--   --  ^do this code last



-- useNewDb = removeOldDb -- since it will be auto created 
-- removeOldDb = callCommand "rm -rf /home/lazylambda/code/Ace/Ace/test-app/dbReal"
-- useSetupDb = do
--   removeOldDb
--   callCommand "cp -r /home/lazylambda/code/Ace/Ace/dbReal_4Users_Test /home/lazylambda/code/Ace/Ace/test-app/dbReal"

-- openPageD p = openPage p >> delay 3  

-- namedButton :: T.Text -> XPathB 
-- namedButton name = ("button", ("name", name))

-- geckodriver = $(staticWhich "geckodriver")
-- --chromedriver = $(staticWhich "chromedriver")
-- chromeBinary' = $(staticWhich "google-chrome-stable")
-- seleniumServer = $(staticWhich "selenium-server")
-- java = $(staticWhich "java")
-- baseUrl = "http://localhost:8020" 

-- type Email = T.Text 
-- withUser :: Email -> WD () -> WD ()
-- withUser email perform = do
--   loginAce email password
--   perform
  

-- tryToss :: forall m. MonadIO m => IO () -> m ()
-- tryToss m = do
--   e :: (Either SomeException ()) <- liftIO $ CE.try $ m
--   when (isLeft e) $ liftIO $ print e
--   pure ()

-- tryTossLifted :: forall b m. (Lifted.MonadBaseControl IO m, MonadIO m) => m () -> m ()
-- tryTossLifted m = do
--   e :: (Either SomeException ()) <- Lifted.try $ m
--   pure ()




-- startSelenium = do
--   tryToss $ callCommand "pkill firefox"
--   --callCommand $ "export PATH=$PATH:" <> chromedriver <> "/bin"
--   --callCommand $ java <> " -jar " <> seleniumServer <> "/selenium-server-*.jar"
--   --callCommand $ java <> " -jar " <> seleniumServer -- <> "/selenium-server-*.jar"
--   callCommand $ seleniumServer <> " &"

-- lower :: String -> String
-- lower = fmap toLower

-- click' :: (MonadThrow m, MonadIO m, SessionState s) => s -> Clickable -> m (String, s) 
-- click' = Scrappy.Requests.click "unexpected-file" 

-- -- | even though this is unsafe this should not leave ace local host
-- mkClickableUnsafe :: ElementRep e => e a -> Maybe Clickable
-- mkClickableUnsafe = mkClickable True (Link "http://localhost:8020/main/auth/login")


-- --newtype ScrappyT m a = ScrappyT { runScrappyT :: MaybeT m a } 
-- printT :: (MonadIO m, Show a) => a -> m ()
-- printT = liftIO . print

-- failedText = "<button class=\"flex w-full rounded-lg bg-[#00B9DA] font-semibold justify-center text-white font-[Sarabun] px-8 sm:px-12 py-5 mt-8 sm:mt-24 mx-auto\">Log in</button>"

-- -- ThisIsForTestingAce123!
-- -- Proton: user='testing-ace2', pass= 'ThisIsForTestingAce123!'



-- email = "testing-ace2@proton.me"
-- password = "ThisIsForTestingAce123!"
-- orgName = "FakeOrg"
-- userEmail1 = "testing-ace2-user1@proton.me"
-- userEmail2 = "testing-ace2-user2@proton.me"
-- userEmail3 = "testing-ace2-user3@proton.me"

-- type XPathB = (T.Text, (T.Text, T.Text))

-- findElemX :: WebDriver wd => T.Text -> wd Element
-- findElemX = findElem . ByXPath 

-- -- | Good typing for most xpath expressions
-- findElemX' :: WebDriver wd => XPathB -> wd Element
-- findElemX' = findElem . ByXPath . toXPath

-- findElemsX :: WebDriver wd => T.Text -> wd [Element]
-- findElemsX = findElems . ByXPath 

-- findElemsX' :: WebDriver wd => XPathB -> wd [Element]
-- findElemsX' = findElems . ByXPath . toXPath 

-- toXPath :: (T.Text, (T.Text,T.Text)) -> T.Text
-- toXPath (hTag, (attrName, attrVal)) = mconcat [ "//", hTag,  "[@", attrName,  "='" , attrVal, "']"]


-- delay :: MonadIO m => Int -> m ()
-- delay = liftIO . threadDelay . (*one_second) 

-- type Microseconds = Int
-- one_second :: Microseconds 
-- one_second = 1000000

-- createAllAccounts = undefined
--   -- doCompanySignup
--   -- clickLinkInEmail email 
--   -- setPasswordsAce password
--   -- --  loginAce email "Warhawks58!" 
--   -- addUsers
--   -- onboard all users with respective names 


-- doCompanySignup :: WD ()
-- doCompanySignup = do
--   openPage "http://localhost:8020/main/admin/new-company-signup?code=edcbcc2a105729f1679c47f57a14df44d9a54bdf8b9962c35c5108c698b8228a7cacc696fd5f2be05b208cf9362d7a1c57da0bbfc2e9403093ea02a8b63f365f"
--   email1 <- findElem (ByXPath "//input[@placeholder='Enter your email']")
--   email2 <- findElem (ByXPath "//input[@placeholder='Re-enter your email']")
--   orgNameEl <- findElem (ByXPath "//input[@placeholder='Organization Name']")
--   sendKeys email email1
--   sendKeys email email2
--   sendKeys orgName orgNameEl
--   findElem (ByXPath "//input[@type='checkbox']") >>= WD.click
--   let bXpath = "//button[@class='focus:outline-none w-full p-4 mt-16 shadow-button bg-[#00B9DA]  font-[Sarabun] font-bold text-white text-body text-center rounded-xl  hover:bg-primary-rich active:bg-primary-desaturated  focus:ring-4 ring-primary ring-opacity-50  transition-all duration-300 ease-in-out  transform hover:scale-105 active:scale-95  hover:shadow-md active:shadow-lg']"
--   findElemX bXpath >>= WD.click
--   liftIO $ threadDelay $ 10 * one_second

-- loginAce :: T.Text -> T.Text -> WD ()
-- loginAce thisEmail thisPass = do
--   openPage "http://localhost:8020/main/auth/login"
--   delay 2
--   findElemX' ("input", ("placeholder", "Enter your email")) >>= sendKeys thisEmail
--   findElemX' ("input", ("placeholder", "Enter your password")) >>= sendKeys thisPass
--   findElemX' ("button", ("class", "flex w-full rounded-lg bg-[#00B9DA] font-semibold justify-center text-white font-[Sarabun] px-8 sm:px-12 py-5 mt-8 sm:mt-24 mx-auto"))
--     >>= WD.click

-- signoutProton :: WD ()
-- signoutProton = do
--   findElemX' ("button", ("data-testid", "heading:userdropdown")) >>= WD.click
--   findElemX' ("button", ("data-testid", "userdropdown:button:logout")) >>= WD.click 

-- clickLinkInEmail :: T.Text -> WD ()
-- clickLinkInEmail email = do
--   let loginURL = "https://account.proton.me/login"
--   openPage loginURL
--   delay 3
--   getCurrentURL >>= \case
--     loginURL -> pure ()
--     _ -> signoutProton >> delay 3
--   liftIO $ threadDelay $ 5 * one_second
--   findElemX "//input[@id='username']" >>= sendKeys email
--   findElemX "//input[@id='password']" >>= sendKeys password
--   findElemX "//button[@class='button w-full button-large button-solid-norm mt-6']" >>= WD.click
--   delay 10
--   -- as you can see i said fuck it; this forces the last email to be the one getting opened 
--   forM_ [1..3] $ \_ -> do 
--     findElemsX "//div[@class='item-container-wrapper relative border-bottom border-weak']" >>= WD.click . head
--     delay 1 
--     back
--   delay 1 
--   findElemsX "//div[@class='item-container-wrapper relative border-bottom border-weak']" >>= WD.click . head
--   delay 5
--   -- printT . length =<< findElemsX "//iframe[@title='Email content']"
--   -- printT . length =<< findElemsX' ("article", ("class", "message-container mx-2 my-4 relative"))
--   -- printT . length =<< findElemsX' ("article", ("data-shortcut-target", "message-container"))
--   findElemsX' ("article", ("data-shortcut-target", "message-container")) >>= WD.click . last
--   -- printT . length =<< findElemsX' ("iframe", ("title", "Email content"))
--   findElemsX' ("iframe", ("title", "Email content")) >>= focusFrame . WithElement . head
--   getSource >>= \html -> do
--     case flip scrapeFirst' (T.unpack html) $ hrefParser of
--     --case flip scrape (T.unpack html) $ hrefParser of 
--       Nothing -> error "no link found"
--       Just l -> do
--         printT l
--         -- streamEdit: needed due to bug with Obelisk link only affecting local builds
--         openPage l
--         -- tryTossLifted $ openPage l
--         -- url8020 <- getCurrentURL
--         -- openPage $ streamEdit (string ":8020") (const ":8000") url8020

-- setPasswordsAce :: T.Text -> WD ()
-- setPasswordsAce tPassword = do
--   findElemX' ("input", ("placeholder", "Enter your new password")) >>= sendKeys tPassword
--   findElemX' ("input", ("placeholder", "Re-enter your new password")) >>= sendKeys tPassword
--   findElemX' ("button", ("class", "focus:outline-none w-full p-4 mt-16 shadow-button bg-primary  font-facit font-bold text-white text-body text-center rounded  hover:bg-primary-rich active:bg-primary-desaturated  focus:ring-4 ring-primary ring-opacity-50"))
--     >>= WD.click
--   delay 4
--   pure ()

-- addUsers :: WD ()
-- addUsers = do
--   findElemX' ("input", ("placeholder","example1@gmail.com, example2@gmail.com, example3@gmail.com, ..."))
--     >>= sendKeys (userEmail1 <> "," <> userEmail2 )
--   findElemX' ("button", ("class", "focus:outline-none w-full p-4 mt-16 shadow-button bg-[#00B9DA]  font-[Sarabun] font-bold text-white text-body text-center rounded-xl  hover:bg-primary-rich active:bg-primary-desaturated  focus:ring-4 ring-primary ring-opacity-50  transition-all duration-300 ease-in-out  transform hover:scale-105 active:scale-95  hover:shadow-md active:shadow-lg"))
--     >>= WD.click

-- -- | Note: userX is AcctID = x
-- -- | TODO: users pick different options (for now theres almost no possibility of this causing issues)


-- doOnboardingForm :: T.Text -> WD ()
-- doOnboardingForm name = do
--   findElemX' ("input", ("class", "w-full px-4 py-3 border border-gray-300 rounded-lg focus:outline-none focus:border-[#00B9DA] font-[Sarabun] text-lg mb-5"))
--     >>= sendKeys name 
--   findElemX' ("button", ("class", "rounded-lg bg-[#00B9DA] font-semibold text-white font-[Sarabun] px-8 py-5 mt-8 ml-10 md:px-48 md:py-5 md:mt-10 mr-30 pr-5"))
--     >>= WD.click

-- -- From homepage 
-- startNewMockInterview :: WD ()
-- startNewMockInterview = do
--   openPage $ "http://localhost:8020" <> "/main/home"
--   findElemsX' ("a", ("class", "py-2 mt-10 flex justify-center w-1/2 block bg-[#2E3A59] text-white px-6 rounded-xl hover:bg-[#00B9DA] font-bold font-sans transition-colors duration-300"))
--     >>= \(_:e:_:[]) -> WD.click e
--   delay 3
--   findElemX' ("a",("class","text-center float-right text-white bg-[#00B9DA] font-bold font-[Sarabun] rounded-[12px] py-6 px-40")) >>= WD.click
--   delay 20
--   pure ()

-- doSomeMockInterviewQuestions :: Int -> WD ()
-- doSomeMockInterviewQuestions n = do
--   forM_ [0..n] $ \_ -> do
--     randomLength <- randomRIO (0, 300)
--     properClickSequence randomLength
--     delay 2

--   where
--     clickImReady = findElemX' (namedButton "Im Ready") >>= WD.click
--     clickEndAnswer = findElemX' (namedButton "End Answer") >>= WD.click
--     clickPlayback = findElemX' (namedButton "Playback") >>= WD.click
--     clickNextQuestion = findElemX' (namedButton "Next Question") >>= WD.click
--     skipQuestion = findElemX' (namedButton "Skip Question") >>= WD.click

--     properClickSequence timeDelay =
--       when (timeDelay > 150) skipQuestion
--       >> clickImReady
--       >> delay timeDelay
--       >> clickEndAnswer
--       >> delay 5
--       >> clickPlayback
--       >> clickNextQuestion



      
-- reviewAllQuestions :: Int -> WD ()
-- reviewAllQuestions userID = do
--   openPage $ baseUrl <> "/lauren/cookies"
--   delay 2
--   let
--     p' :: ScraperT Int
--     p' = fmap (read . innerText') $ el "td" []
--     p = (,,) <$> p' <*> p' <*> p'
--   getSource >>= pure . scrape (el "tr" [] `containsFirst` p) . T.unpack >>= \case
--     Nothing -> pure ()
--     Just (tableRows) -> do
--       let
--         getID uid = Data.List.find (\(userID,_,_) -> userID == uid) tableRows
--       case getID userID of
--         Nothing -> error "No user ID"
--         Just (_, notReady, ready)
--           | ready == 0 && notReady == 0 -> error "no questions shown"
--           | notReady == 0 && ready /= 0 -> do
--               performReview userID ready
              
--           | notReady /= 0 -> delay 10 >> reviewAllQuestions userID
--           | otherwise -> error "uncaught case"
        
--       pure ()

-- clickXPath byX = findElemX' byX >>= WD.click
-- sendKeysXPath byX keys = findElemX' byX >>= sendKeys keys
-- namedTextbox name = ("textarea", ("name", name))
                         
-- -- w-full bg-black px-4 text-3xl border border-gray-300 rounded-lg focus:outline-none focus:border-[#00B9DA] font-[Sarabun] mb-10
-- -- w-full bg-black px-4 text-3xl border border-gray-300 rounded-lg focus:outline-none focus:border-[#00B9DA] font-[Sarabun] mb-10
-- performReview :: Int -> Int -> WD ()
-- performReview userID numQuestions = do
--   replicateM_ numQuestions $ do
--     findElemX' (namedTextbox "User ID") >>= sendKeys (T.pack $ show userID)
--     clickXPath $ namedButton "Start Reviewing this User ID"
--     -- check video is working 
--     -- place selenium stamps
--     sendKeysXPath (namedTextbox "Summary") "SELENIUM-WAS-HERE"
--     -- submit , repeat
--     findElemX' (namedButton "Complete Review of this Question") >>= WD.click

-- -- 10 videos over 2 reports
-- -- use offensive name
-- user1 :: WD () 
-- user1 = do
--   let email = userEmail1
--   -- NOTE: this input is as offensive as it gets, but we need to ensure the worst is definitively stopped
--   offensiveName <- liftIO $ readFile "offensive-input" 
--   clickLinkInEmail email
--   setPasswordsAce password
--   doOnboardingForm $ T.pack offensiveName
--   startNewMockInterview 
--   doSomeMockInterviewQuestions 6
--   startNewMockInterview 
--   doSomeMockInterviewQuestions 4

--   -- we can do this still cuz the auth is different for reviewers
--   reviewAllQuestions 1

--   -- when we review we will always put TEST-SELENIUM in one of the manual fields so we can check that we got
--   -- the feedback properly 
--   -- readAllQuestions
--     -- check video plays properly
--   -- checkProfile
--     --changeUsersStream 

--   -- tryDisallowedAdminActions

--   -- testNavBar
--   --   -- check tutorial plays 

--   -- -- ask for email password as input

--   -- doMockInterviewWrong

--   -- checkRecorderTimeout 
--   -- user submission
--   -- activity logs
--   --

  

--   pure () 

-- -- just 1 video
-- -- use ok but questionable name: Nigerian-Mamba
-- user2 = do
--   let email = userEmail2
--   clickLinkInEmail email
--   setPasswordsAce password
--   doOnboardingForm "Nigerian-Mamba"
--   pure ()

-- -- just onboard
-- -- Use basic name 
-- user3 = do
--   let email = userEmail3
--   clickLinkInEmail email
--   setPasswordsAce password
--   doOnboardingForm "Chris the Guy"
--   -- do report 1
--   -- do exercises
--   -- do report 2
--   pure ()

-- -- | user4 is through subscription and not connected to other users 
-- user4 = undefined
--   -- resubscribe before performing actions



-- --http://localhost:8000/reset/xieafvJ1M8M84lccl5Yv48QUBGfVxgIfl0wvWoclZJ+U3iVKjGM88eDpoTaMljlaPApWIIMNmNT9mwm68k1U8bRLAFf4O1E5rrYyYBpvws+mjFs4NPnZbsgf%2Fd0fe9Bbm%2FH7wVOFau4eK+kXgPYQQOKAGLTzNtwuJjxow42%2FCYV4dDQa3mZ5IKqblaHxtqM8WMoF

data APIProvider = OpenAI | Google | AWS
-- Define the APIKey type with a phantom type parameter
newtype APIKey (api :: APIProvider) = APIKey { unAPIKey :: T.Text }

-- Create an APIKey for OpenAI
-- openAIKey :: APIKey 'OpenAI
-- openAIKey = APIKey "your-openai-api-key"

-- -- Create an APIKey for Google
-- googleKey :: APIKey 'Google
-- googleKey = APIKey "your-google-api-key"

-- -- Create an APIKey for AWS
-- awsKey :: APIKey 'AWS
-- awsKey = APIKey "your-aws-api-key"


-- withReviewerAuth :: T.Text -> Snap (Either ApiError a) -> Snap (Either ApiError a)
-- withReviewerAuth pass m = do
--   case elem pass (snd <$> reviewerEmailAndCodes) of
--     False -> pure $ Left "Bad password"
--     True -> m

-- inReportedTime :: MonadIO m => m a -> m a
-- inReportedTime m = do
--   t1 <- liftIO getCurrentTime
--   x <- m
--   t2 <- liftIO getCurrentTime
--   liftIO $ print $ diffUTCTime t2 t1
--   pure x


-- -- | Redirect to the specified frontend route
-- frontendRedirect :: MonadSnap m => T.Text -> R FrontendRoute -> m ()
-- frontendRedirect baseRoute dest = do
--   let href = T.encodeUtf8 $ (baseRoute <>) $ renderFrontendRoute checkedFullRouteEncoder dest
--   liftSnap $ Snap.redirect href

-- -- IO so that we can get randomization
-- shuffle :: [a] -> IO [a]
-- shuffle [] = pure []
-- shuffle xs = do
--   picked <- randomRIO (1, length xs) -- so that the length of the first is always 1+
--   let (x:pre, secnd) = splitAt picked xs -- picked is also the length of the first
--   xs' <- shuffle $ secnd <> pre
--   pure $ x : xs'

-- shuffleWell :: [a] -> IO [a]
-- shuffleWell (a:b:[]) = randomRIO (1,10) >>= \x -> if (x :: Int) < 6 then pure (a:b:[]) else pure (b:a:[] )
-- shuffleWell xs = shuffle =<< pure . reverse =<< shuffle xs

-- getRequestBody :: MonadSnap m => m LBS.ByteString
-- getRequestBody = LBS.fromChunks <$> runRequestBody Streams.toList

-- emailSend = EmailSendConfig "Ace Interview Prep" "galen@aceinterviewprep.io"
-- emailSendFrom = Address
--   { addressName = Just $ _emailSendConfig_name emailSend
--   , addressEmail = _emailSendConfig_address emailSend
--   }

-- -- TODO: withEmailConfig :: T.Text -> IO ()
-- emailLauren :: MonadIO m => EmailConfig -> LT.Text -> m ()
-- emailLauren cfg msg = do
--   let
--     to = Address
--          { addressName = Just ("Lauren DeSouza" :: T.Text)
--          , addressEmail = "lauren@aceinterviewprep.io"
--          } -- recipients Address
--     mail = simpleMail' to emailSendFrom (T.take 20 (LT.toStrict msg) <> "...") msg
--   void $ sendEmailIfNotLocal cfg mail

-- emailUser
--   :: MonadIO m
--   => EmailConfig
--   -> ConnectionPool
--   -> AcctID
--   -> T.Text
--   -> (T.Text -> T.Text)
--   -- -> (T.Text -> (T.Text, T.Text))
--   -> T.Text
--   -> m (Either T.Text ())
-- emailUser cfg dbConn (AcctID uid) subject toMsg html = liftIO $ do
--   (name, email) <- runSerializable dbConn $ do
--     n <- getSelfUsername (AcctID uid)
--     e <- getUsersEmail (AcctID uid)
--     pure (n, e)
--   print "got here"
--   case email of
--     Nothing -> pure . Left $ "Email not found for userID " <> (T.pack $ show uid)
--     Just email' -> do
--       let
--           to = Address
--             { addressName = name
--             , addressEmail = email'
--             } -- recipients Address
--           message = (LT.fromStrict $ toMsg $ fromMaybe "Ace User" name)
--       let mail = simpleMail' to emailSendFrom subject message
--       --mail <- simpleMail to emailSendFrom subject "hey1" "hey2" []
--       sendEmailIfNotLocal cfg mail
--       --flip runEmailT cfg $ widgetMail widgetEmailConfig (email' NonEmpty.:| []) subject (Just $ pure "hey im text") (text "")
--       --pure $ mapLeft (T.pack . show) x


-- --widgetEmailConfig = WidgetEmailCfg "Ace Interview Prep" "galen@aceinterviewprep.io" "aceinterviewprep.app" checkedFullRouteEncoder

-- -- | For use in emailGalenOnLeft
-- emailGalen :: MonadIO m => EmailConfig -> LT.Text -> m ()
-- emailGalen cfg msg = do
--   let
--       to = Address
--         { addressName = Just ("Fart Face" :: T.Text)
--         , addressEmail = "galen@aceinterviewprep.io"
--         } -- recipients Address
--       mail = simpleMail' to emailSendFrom ((LT.toStrict msg) <> "...") msg
--   void $ sendEmailIfNotLocal cfg mail

-- sendEmailIfNotLocal :: MonadIO m => EmailConfig -> Mail -> m (Either T.Text ())
-- sendEmailIfNotLocal cfg mail = do
--   route <- liftIO $ readFile "config/common/route"
--   case isPrefixOf "http://localhost:" route of
--     True -> do
--       let
--         showParts = \case { PartContent bs -> T.unpack . T.decodeUtf8 . LBS.toStrict $ bs ; _ -> ""} . partContent
--       --let showParts = fmap  . mconcat . mailParts
--       liftIO . print . mailTo $ mail
--       liftIO . print . mconcat . fmap showParts . mconcat . mailParts $ mail
--       pure $ Right ()
--     False -> do
--       x :: (Either IOException (Either EmailError ())) <- liftIO $ CE.try $ sendEmail cfg mail
--       liftIO $ print x
--       case x of
--         Left ioException -> pure $ Left (T.pack $ show ioException)
--         Right (Left emailError) -> pure $ Left (T.pack $ show emailError)
--         Right (Right ()) -> pure $ Right ()

-- -- | Rewrite with getRequeSON a, ToJSON b, Show b)
-- --   => T.Text
-- --   -> ConnectionPool
-- --   -> Web.ClientSession.Key
-- --   -> EmailConfig
-- --   -- -> (a -> Snap.Snap (Either ApiError b))
-- --   -> (AcctID -> ConnectionPool -> a -> Snap.Snap (Either ApiError b))
-- --   -> Snap.Snap ()
-- -- withPrivateJSONRequestResponse route dbConn key emailConfig withF = privateRoute route dbConn key $ \acctId -> do
-- --   x :: Either ApiError b <- withJSONRequestResponse emailConfig (withF acctId dbConn)
-- --   when (isLeft x) $ liftIO $ do
-- --     emailGalen emailConfig (LT.fromStrict . T.pack $ show acctId <> show x)
-- --     print (LT.fromStrict . T.pack $ show acctId <> show x)



-- withConstrainedPrivateJSONRequestResponse
--   :: forall a b. (FromJSON a, ToJSON b, Show b)
--   => T.Text
--   -> ConnectionPool
--   -> Web.ClientSession.Key
--   -> EmailConfig
--   -> NonEmpty.NonEmpty UserType
--   -- -> (a -> Snap.Snap (Either ApiError b))
--   -> (AcctID -> ConnectionPool -> a -> Snap.Snap (Either ApiError b))
--   -> Snap.Snap ()
-- withConstrainedPrivateJSONRequestResponse route dbConn key emailConfig allowedUserTypes withF = do
--   constrainedPrivateRoute route dbConn key allowedUserTypes $ \acctId -> do
--     x :: Either ApiError b <- withJSONRequestResponse emailConfig (withF acctId dbConn)
--     when (isLeft x) $ liftIO $ do
--       emailGalen emailConfig (LT.fromStrict . T.pack $ show acctId <> show x)
--       print (LT.fromStrict .stBodyJSON
-- withRequestBodyJSON :: forall a. Aeson.FromJSON a => (a -> Snap.Snap ()) -> Snap.Snap ()
-- withRequestBodyJSON withF = do
--   raw <- getRequestBody
--   case Aeson.eitherDecode raw :: Either String a of
--     Left e -> writeJSON' $ Left $ T.pack e
--     Right x -> withF x

-- withPublicJSONRequestResponse
--   :: forall a b. (Aeson.FromJSON a, Aeson.ToJSON b, Show b)
--   => ConnectionPool
--   -> EmailConfig
--   -> (ConnectionPool -> a -> Snap.Snap (Either ApiError b))
--   -- -> (AcctID -> ConnectionPool -> a -> Snap.Snap (Either ApiError b))
--   -> Snap.Snap ()
-- withPublicJSONRequestResponse dbConn emailConfig withF = do
--   x :: Either ApiError b <- withJSONRequestResponse emailConfig (withF dbConn)
--   when (isLeft x) $ liftIO $ do
-- --    print "hey"
--     --print (LT.fromStrict . T.pack $ show x)
--     emailGalen emailConfig (LT.fromStrict . T.pack $ show x)
--     pure ()

-- -- withPrivateJSONRequestResponse
-- --   :: forall a b. (FromJSON a, ToJSON b, Show b)
-- --   => T.Text
-- --   -> ConnectionPool
-- --   -> Web.ClientSession.Key
-- --   -> EmailConfig
-- --   -- -> (a -> Snap.Snap (Either ApiError b))
-- --   -> (AcctID -> ConnectionPool -> a -> Snap.Snap (Either ApiError b))
-- --   -> Snap.Snap ()
-- -- withPrivateJSONRequestResponse route dbConn key emailConfig withF = privateRoute route dbConn key $ \acctId -> do
-- --   x :: Either ApiError b <- withJSONRequestResponse emailConfig (withF acctId dbConn)
-- --   when (isLeft x) $ liftIO $ do
-- --     emailGalen emailConfig (LT.fromStrict . T.pack $ show acctId <> show x)
-- --     print (LT.fromStrict . T.pack $ show acctId <> show x)



-- withConstrainedPrivateJSONRequestResponse
--   :: forall a b. (FromJSON a, ToJSON b, Show b)
--   => T.Text
--   -> ConnectionPool
--   -> Web.ClientSession.Key
--   -> EmailConfig
--   -> NonEmpty.NonEmpty UserType
--   -- -> (a -> Snap.Snap (Either ApiError b))
--   -> (AcctID -> ConnectionPool -> a -> Snap.Snap (Either ApiError b))
--   -> Snap.Snap ()
-- withConstrainedPrivateJSONRequestResponse route dbConn key emailConfig allowedUserTypes withF = do
--   constrainedPrivateRoute route dbConn key allowedUserTypes $ \acctId -> do
--     x :: Either ApiError b <- withJSONRequestResponse emailConfig (withF acctId dbConn)
--     when (isLeft x) $ liftIO $ do
--       emailGalen emailConfig (LT.fromStrict . T.pack $ show acctId <> show x)
--       print (LT.fromStrict . T.pack $ show acctId <> show x)

-- withDependentPrivateJSONRequestResponse
--   :: forall a b. (FromJSON a, ToJSON b, Show b)
--   => T.Text
--   -> ConnectionPool
--   -> Web.ClientSession.Key
--   -> EmailConfig
--   -> (UserType -> AcctID -> ConnectionPool -> a -> Snap.Snap (Either ApiError b))
--   -> Snap.Snap ()
-- withDependentPrivateJSONRequestResponse route dbConn key emailConfig withF = do
--   dependentPrivateRoute route dbConn key $ \userType acctId -> do
--     x :: Either ApiError b <- withJSONRequestResponse emailConfig (withF userType acctId dbConn)
--     when (isLeft x) $ liftIO $ do
--       emailGalen emailConfig (LT.fromStrict . T.pack $ show acctId <> show x)
--       print (LT.fromStrict . T.pack $ show acctId <> show x)

-- -- | This function is meant to be completely independent of Authentication, so that it can be
-- -- | wrapped by different auth schemes
-- withJSONRequestResponse :: forall a b. (FromJSON a, ToJSON b, Show b)
--                         => EmailConfig
--                         -> (a -> Snap.Snap (Either ApiError b))
--                         -> Snap.Snap (Either ApiError b)
-- withJSONRequestResponse = theoreticalJSONMonad

-- withPrivateRequestResponse
--   :: Show a
--   => EmailConfig
--   -> AcctID
--   -> (AcctID -> LBS.ByteString -> Snap.Snap (Either ApiError a))
--   -> Snap.Snap (Either ApiError a)
-- withPrivateRequestResponse = withPrivateErrorHandling

-- withPrivateErrorHandling
--   :: Show a
--   => EmailConfig
--   -> AcctID
--   -- -> T.Text
--   -- -> ConnectionPool
--   -- -> Web.ClientSession.Key
--   -> (AcctID -> LBS.ByteString -> Snap.Snap (Either ApiError a))
--   -> Snap.Snap (Either ApiError a)
-- withPrivateErrorHandling emailConfig acctID withF = do
--   raw <- getRequestBody
--   x <- catch (withF acctID raw) (\(SomeException e) -> pure $ Left $ T.pack . show $ e)
--   when (isLeft x) $ liftIO $ do
--     emailGalen emailConfig (LT.fromStrict . T.pack $ show x)
--     print (LT.fromStrict . T.pack $ show x)
--     log acctID (LT.fromStrict . T.pack $ show x)
--   pure x

-- -- | NOTE: not yet in use, may not be ever needed but do keep as generic interface
-- withPublicRequestResponse
--   :: Show a => EmailConfig
--   -> (LBS.ByteString -> Snap.Snap (Either ApiError a))
--   -> Snap.Snap (Either ApiError a)
-- withPublicRequestResponse = withPublicErrorHandling
-- -- | NOTE: not yet in use, may not be ever needed but do keep as generic interface
-- withPublicErrorHandling
--   :: Show a => EmailConfig
--   -> (LBS.ByteString -> Snap.Snap (Either ApiError a))
--   -> Snap.Snap (Either ApiError a)
-- withPublicErrorHandling emailConfig withF = do
--   raw <- getRequestBody
--   x <- catch (withF raw) (\(SomeException e) -> pure $ Left $ T.pack . show $ e)
--   when (isLeft x) $ liftIO $ do
--     emailGalen emailConfig (LT.fromStrict . T.pack $ show x)
--     logNoAuth (LT.fromStrict . T.pack $ show x)
--   pure x


-- --privateHandler = privateRoute'

-- -- TODO(Galen): use requestBodyJSON
-- theoreticalJSONMonad
--   :: forall a b. (FromJSON a, ToJSON b, Show b)
--   => EmailConfig
--   -> (a -> Snap.Snap (Either ApiError b))
--   -> Snap.Snap (Either ApiError b)
-- theoreticalJSONMonad emailConfig withF = do
--   raw <- getRequestBody
--   x :: Either ApiError b <- case Aeson.eitherDecode raw :: Either String a of
--     Left e -> pure $ Left $ T.pack e
--     Right good -> catch (withF good) (\(SomeException e) -> pure $ Left $ T.pack . show $ e)
--   --when (isLeft x) $ liftIO $ emailGalen emailConfig (LT.fromStrict . T.pack . show $ x)
--   writeJSON x
--   pure x

-- getRequestBodyJSON :: forall a. FromJSON a => Snap.Snap (Either String a)
-- getRequestBodyJSON = Aeson.eitherDecode <$> getRequestBody

-- getHeader :: MonadSnap m => T.Text -> m (Maybe BS.ByteString)
-- getHeader headerName = Snap.getHeader (mk $ T.encodeUtf8 headerName) <$> Snap.getRequest

-- -- TODOMaybe(galen): Parameterize with file type
-- -- by getting the content-type header

-- mapLeft :: (a -> b) -> Either a c -> Either b c
-- mapLeft f = bimap f Prelude.id

-- logFrontend :: (MonadIO m, Show a) => AcctID -> a -> m ()
-- logFrontend aid a = log aid ("Frontend", a)


-- log :: (MonadIO m, Show a) => AcctID -> a -> m ()
-- log (AcctID aid) a = liftIO $ do
--   let newLog = show a
--   putStrLn newLog
--   let dir = "db/logs/" <> (show aid)
--   liftIO $ createDirectoryIfMissing True dir
--   t <- liftIO $ getCurrentTime
--   let date = take 10 $ show t
--   liftIO $ appendFile (dir </> date) $ show (drop 11 $ show t, newLog) <> "\n"

-- catchToss :: IO a -> IO ()
-- catchToss m = catch (m >> pure ()) (\(_ :: IOException) -> pure ())

-- logNoAuth a = catchToss $ do
--   let newLog = show a
--   putStrLn newLog
--   let dir = "db/logs/unauthenticated"
--   liftIO $ createDirectoryIfMissing True dir
--   t <- liftIO $ getCurrentTime
--   let date = take 10 $ show t

--   liftIO $ appendFile (dir </> date) $ show (drop 10 $ show t, newLog) <> "\n"



-- data MyError = MyError { error_ :: T.Text } deriving (Generic)

-- data WithError a = WithError { response :: Either T.Text a } deriving (Generic)

-- -- | Get and parse a json configuration
-- getJsonConfigBase :: (HasConfigs m, FromJSON a) => T.Text -> m (Maybe (Either String a))
-- getJsonConfigBase = (fmap.fmap) (eitherDecodeStrict') . getConfig

-- -- | Get and parse a json configuration
-- getJsonConfig :: (MonadFail m, HasConfigs m, FromJSON a) => T.Text -> m a
-- getJsonConfig key = getJsonConfigBase key >>= \case
--   Nothing -> fail $ "getJsonConfig missing key: " <> T.unpack key
--   Just (Left err) -> fail $ "getJsonConfig invalid: " <> err
--   Just (Right val) -> pure val

-- writeJSON' :: Either T.Text () -> Snap.Snap ()
-- writeJSON' = writeJSON

-- -- TODO: show how to use Jobs
-- -- TODO: serveDbOverWebsockets impl.


-- -- updateSnapConfig :: Config m a -> Config m a
-- -- updateSnapConfig cfg = cfg {
-- --                            }


-- -- From visual analysis branch
-- newtype Column a = Column { unColumn :: [a] } deriving (Show, Ord, Eq, Functor, Foldable)
-- getColumn :: Int -> [[a]] -> Maybe (Column a)
-- getColumn col csv = fmap Column $ sequenceA $ fmap (f col) csv
--   where f col row = if length row - 1 < col then Nothing else Just $ row !! col
-- average :: (Fractional a) => [a] -> a
-- average xs = (\(tot,count) -> tot / count ) $ foldr avg (0,0) xs
-- avg :: (Num a, Num b, Num c) => a -> (a,c) -> (a,c)
-- avg a (b, count) = (a + b, count + 1)

-- -- | Should likely change to is bootcamp user or inbound client
-- isPaidForEmail :: T.Text -> IO Bool
-- isPaidForEmail email = do
--   eithCSV <- parseCSVFromFile "config/backend/registeredEmails.csv"
--   case eithCSV of
--     Left _ -> error "unrecoverable error: no registeredEmails.csv"
--     Right csv ->
--       case fmap (elem . T.toLower $ email) $ ((fmap.fmap) (T.toLower . T.pack) $ getColumn 0 csv) of
--         Nothing -> error "unrecoverable error: malformed registeredEmails.csv"
--         Just booly -> pure booly


-- tryToss ::  IO () -> IO ()
-- tryToss m = do
--   e :: (Either SomeException ()) <- liftIO $ CE.try $ m
--   when (isLeft e) $ print e
--   pure ()

-- -- todo: use readerT for Manager and ApiKey
-- -- then create this monad as a newtype with getter funcs
-- -- and a put for history
-- type MonadGPT m a = StateT ConversationHistory m a
-- type ConversationHistory = [GPTQuery T.Text]
-- data GPTQuery a = GPTQuery
--   { _gptQuery_tag :: Tag
--   , _gptQuery_question :: GPTQuestion
--   , _gptQuery_answer ::  GPTAnswer a
--   }
-- newtype Tag = Tag { unTag :: T.Text } deriving (Eq,Show)
-- newtype GPTQuestion = GPTQuestion [ContentWithRole]
-- newtype GPTAnswer a = GPTAnswer { unGPTAnswer :: a } deriving Show
-- newtype GPTError = GPTError T.Text deriving Show

-- data RelevantContext
--   = LastN Int
--   | Relevants [Tag]
--   | LastNRelevant Int (Tag -> Bool) -- LastN matching pattern; most general

-- renderHistory :: ConversationHistory -> ContentWithRole
-- renderHistory = cwr Assistant . ((<>) "Our conversation history so far:") . T.intercalate "\n" . fmap renderItem
--   where
--     renderItem (GPTQuery _ (GPTQuestion q) (GPTAnswer a)) =
--       "Me: " <> (T.decodeUtf8 . LBS.toStrict . Aeson.encode) q <> "\n" <> "ChatGPT: " <> a

-- getRelevantCtx :: MonadIO m => RelevantContext -> MonadGPT m ConversationHistory
-- getRelevantCtx = \case
--   LastN n -> gets (take n)
--   Relevants tags -> gets (flip finds tags)
--   LastNRelevant n f -> gets (take n . filter (f . _gptQuery_tag))
--   where
--     finds hist tags =
--       catMaybes $ fmap (\t -> find (\h -> t == _gptQuery_tag h) hist) tags


-- testctx :: MonadIO m => MonadGPT m ()
-- testctx = do
--   mgr <- liftIO $ newManager tlsManagerSettings
--   k <- liftIO $ fmap T.pack $ readFile "config/backend/gptAPIKey"
--   r1 <- askGPTWithContext k mgr (LastN 10) (Tag "Name", GPTQuestion [cwr User "my name is galen"])
--   r2 <- askGPTWithContext k mgr (LastN 10) (Tag "Hey", GPTQuestion [cwr User "please tell me what my name is"])
--   r3 :: Either GPTError (GPTAnswer Int) <- askGPTWithContextTyped k mgr (LastN 10) (Tag "Hey", GPTQuestion [cwr User "how many letters in my name"])
--   r4 :: Either GPTError (GPTAnswer Int) <- askGPTWithContextTyped k mgr (Relevants [Tag "Name"]) (Tag "Hey", GPTQuestion [cwr User "how many letters in my name"])
--   r5 :: Either GPTError (GPTAnswer Int) <- askGPTWithContextTyped k mgr (Relevants []) (Tag "Hey", GPTQuestion [cwr User "how many letters in my name"])
--   r6 :: Either GPTError (GPTAnswer Int) <- askGPTWithContextTyped k mgr (LastN 0) (Tag "Hey", GPTQuestion [cwr User "how many letters in my name"])
--   liftIO $ print r1
--   liftIO $ print r2
--   liftIO $ print r3
--   liftIO $ print r4
--   liftIO $ print r5
--   liftIO $ print r6
--   pure ()

-- askGPTWithContextTyped
--   :: forall m a.
--   ( Typeable a
--   , Read a
--   , MonadIO m
--   )
--   => APIKey 'OpenAI
--   -> Manager
--   -> TokenLimit
--   -> RelevantContext
--   -> (Tag, GPTQuestion)
--   -> MonadGPT m (Either GPTError (GPTAnswer a))
-- askGPTWithContextTyped key mgr tokenLimit relCtx (thisTag, GPTQuestion contents) = do
--   liftIO $ print "NEQWWWWWWWWWWWWWWWWWWWWWW"
--   let proxy = Proxy :: Proxy a
--   let returnT = gptReturnType proxy
--   let
--     readEitherText :: Read a => T.Text -> Either T.Text a
--     readEitherText = mapLeft T.pack . readEither2 . T.unpack
--       where
--         p :: Proxy a
--         p = Proxy
--         readEither2 x = case readEither x of
--           Right a -> Right a
--           Left e -> case readEither $ "\"" <> x <> "\"" of
--             Right a -> Right a
--             Left _ -> readEither $ "\"" <> (T.unpack $ escapeText $ T.pack x) <> "\"" -- Left $ "Couldn't read" <> (show $ typeRep p)

--       -- where readEither2 x = case readEither x of
--       --         Left e -> readEither $ "\"" <> escape x <> "\""
--       --         Right a -> Right a

--   ctx <- renderHistory <$> getRelevantCtx relCtx
--   askGPT key mgr tokenLimit (ctx : contents <> returnT) >>= \case
--     Left e -> pure . Left . GPTError $ e
--     Right txt -> case readEitherText txt of
--       Left e -> pure . Left . GPTError $
--         e <> "When reading return type: (x :: "  <> (T.pack . show $ typeRep proxy ) <> ") from base response: " <> txt
--         <> "From Prompt: "
--         <> (T.pack $ show (ctx : contents <> returnT))

--       Right typed -> do
--         let new = GPTQuery thisTag (GPTQuestion contents) (GPTAnswer txt)
--         modify ((:) new)
--         pure . Right . GPTAnswer $ typed

-- askGPTWithContext :: MonadIO m
--   => APIKey 'OpenAI
--   -> Manager
--   -> TokenLimit
--   -> RelevantContext
--   -> (Tag, GPTQuestion)
--   -> MonadGPT m (Either GPTError (GPTAnswer T.Text))
-- askGPTWithContext key mgr maxTokens relCtx (thisTag, GPTQuestion contents) = do
--   histItems <- getRelevantCtx relCtx
--   askGPT key mgr maxTokens (renderHistory histItems : contents) >>= \case
--     Left e -> pure $ Left $ GPTError e
--     Right answer -> do
--       let new = GPTQuery thisTag (GPTQuestion contents) (GPTAnswer answer)
--       modify ((:) new)
--       pure $ Right $ GPTAnswer answer

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- ff :: forall a. Read a => T.Text -> Either T.Text a
-- ff = mapLeft T.pack . readEither2 . T.unpack
--   where readEither2 x = case readEither x of
--           Right a -> Right a
--           Left e -> readEither $ "\"" <> escape x <> "\""


-- -- | TODO: Configure temperature for less variability
-- -- | Todo: we should probably use scrappy here so that we dont care about prefixing/position
-- askGPTTyped
--   :: forall m a.
--   ( MonadIO m
--   , Typeable a
--   , Read a
--   )
--   => APIKey 'OpenAI
--   -> Manager
--   -> TokenLimit
--   -> GPTQuestion
--   -> m (Either GPTError (GPTAnswer a))
-- askGPTTyped apiKey mgr maxTokens (GPTQuestion prompt) = do
--   let proxy = Proxy :: Proxy a
--   let returnT = gptReturnType proxy
--   let
--     readEitherText :: forall a. Read a => T.Text -> Either T.Text a
--     readEitherText = mapLeft T.pack . readEither2 . T.unpack
--       where readEither2 x = case readEither x of
--               Right a -> Right a
--               Left e -> readEither $ "\"" <> escape x <> "\""
--                 --Right a -> Right a


--   r <- askGPT apiKey mgr maxTokens $ prompt <> returnT
--   pure . bimap GPTError GPTAnswer $ readEitherText =<< r


both f = bimap f f

escapeText :: T.Text -> T.Text
escapeText = T.concatMap escapeChar
  where
    escapeChar '\"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c    = T.singleton c


escape :: String -> String
escape = concatMap esc
    where
        escchars :: String
        escchars = "$\\^.*~[]"
        esc c   | c `elem` escchars = ['\\',c]
                | otherwise         = [c]

-- | Should we have a more generic scrape func? ie can use with any parser
--readParser :: (Read a, Stream s m t) => ScraperT m a
--readParser
-- checkTypeFromStringAndPrint :: forall a. (Read a, Typeable a) => Proxy a -> String -> IO Bool
-- checkTypeFromStringAndPrint proxy str = do
--     putStrLn $ "Type: " ++ show (typeRep proxy)
--     return $ maybe False (const True) (readMaybe str :: Maybe a)

-- gptReturnType :: forall a. Typeable a => Proxy a -> [ContentWithRole]
-- gptReturnType proxy =
--   let
--     typeInfo = T.pack $ show (typeRep proxy)
--   in
--     if typeInfo == "Text" || typeInfo == "String"
--     then []
--     else [ cwr System $ "In responding to the above question, give me only the Haskell type:" <> typeInfo <> " and nothing else in your response: Format should be parsable as the Haskell type:" <> typeInfo ]




gptModel :: T.Text
gptModel = "gpt-4o-2024-05-13" -- "gpt-4"






-- Define the data structure for the error object

-- Since we're using a field name that doesn't directly match the JSON (type'), we need to customize the parsing.
-- For other fields, we can rely on Aeson's default behavior.

-- instance FromJSON ErrorObject where
--   parseJSON = withObject "ErrorObject" $ \v -> ErrorObject
--     <$> v .: "message"
--     <*> v .: "type"
--     <*> v .:? "param"
--     <*> v .:? "code"

-- instance FromJSON ErrorResponse where
--   parseJSON = withObject "ErrorResponse" $ \v -> ErrorResponse
--     <$> v .: "error"



































-- {"error\": {\n    \"message\": \"The server had an error processing your request. Sorry about that! You can retry your request, or contact us through our help center at help.openai.com if you keep seeing this error. (Please include the request ID req_00e6e53118888081a9eb7de9b2feb1f6 in your email.)\",\n    \"type\": \"server_error\",\n    \"param\": null,\n    \"code\": null\n  }\n}
data GPTRequestBody = GPTRequestBody
  { model :: T.Text
  -- , response_format :: GPTResponseFormat
  , max_tokens :: Maybe Int 
  , messages :: [ContentWithRole]
  -- more exist but we dont need them or use them
  } deriving (Show,Generic)

data GPTType = GPT_Text | GPT_JSON deriving Show

instance ToJSON GPTType where
  toJSON = toJSON . T.pack . \case
    GPT_Text -> "text"
    GPT_JSON -> "json_object"

instance FromJSON GPTType where
  parseJSON = withText "GPTType" $ \case
    "text" -> pure GPT_Text
    "json_object" -> pure GPT_JSON
data GPTResponseFormat = GPTResponseFormat
  { _gptResponseFormat_type :: GPTType
  } deriving (Generic, Show) 


-- | Shorthand for ContentWithRole
cwr :: GPTRole -> T.Text -> ContentWithRole
cwr = ContentWithRole 

data ContentWithRole = ContentWithRole
  { role :: GPTRole 
  , content :: T.Text
  } deriving (Show, Generic)

data GPTRole = System -- we provide some context: "Pretend you are an Interviewer"
             | User -- we ask some question
             | Assistant deriving (Show, Generic)
             -- We tell GPT something : "The assistant messages help store prior responses.
             -- They can also be written by a developer to help give examples of desired behavior."

instance ToJSON GPTRole where
  toJSON = \case
    System -> String "system"
    User -> String "user"
    Assistant -> String "assistant"

instance FromJSON GPTRole where
  parseJSON = withText "Role" $ \case
    "system" -> pure System
    "user" -> pure User
    "assistant" -> pure Assistant
    x -> fail $ show x

instance ToJSON ContentWithRole
instance FromJSON ContentWithRole

instance ToJSON GPTRequestBody 

-- scrubPrefix :: String -> Options
-- scrubPrefix s =
--   defaultOptions { fieldLabelModifier = drop (length s)
--                  }
data PromptResponse = PromptResponse { id :: T.Text
                                     , object :: T.Text
                                     , created :: Int
                                     , choices :: [ResMessage]
                                     , usage :: Usage
                                     } deriving (Show, Generic)

data ResMessage = ResMessage { message :: ContentWithRole
                             , finish_reason :: T.Text
                             , index :: Int 
                             } deriving (Show,Generic)


data Usage = Usage { prompt_tokens :: Int
                   , completion_tokens :: Int
                   , total_tokens :: Int
                   } deriving (Show,Generic)

instance FromJSON Usage
instance ToJSON Usage
instance FromJSON PromptResponse
instance ToJSON PromptResponse
instance FromJSON ResMessage
instance ToJSON ResMessage


-- Wrapper for Raw GPT Response         
data Content = Content { unContent :: LBS.ByteString } deriving Show

-- default 
data TextToSpeechBody = TextToSpeechBody
  { _textToSpeech_model :: T.Text
  , _textToSpeech_voice :: T.Text
  , _textToSpeech_input :: T.Text
  }

data ErrorOpenAI = ErrorOpenAI {
  _errorOpenAI_message :: T.Text,
  _errorOpenAI_type :: T.Text, -- 'type' is a reserved keyword in Haskell, so we use type' or another name
  _errorOpenAI_param :: Maybe T.Text,
  _errorOpenAI_code :: Maybe T.Text
} deriving (Show, Generic)

-- Define the data structure for the top-level object
data ErrorResponseOpenAI = ErrorResponseOpenAI {
  _errorResponseOpenAI_error :: ErrorOpenAI
} deriving (Show, Generic)

deriveJSON (scrubPrefix "_errorOpenAI_") ''ErrorOpenAI
deriveJSON (scrubPrefix "_errorResponseOpenAI_") ''ErrorResponseOpenAI
deriveJSON (scrubPrefix "_textToSpeech_") ''TextToSpeechBody
deriveJSON (scrubPrefix "_gptResponseFormat_") ''GPTResponseFormat

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests where 

-- Idea: a language extension that allows module organization like:

-- import Control
         -- .Monad
           -- .IO.Class (liftIO)
           -- .Trans
             -- .StateT (f)
             -- .ExceptT (g)
import Proxies (mkProxdManager)
import BuildActions (FilledForm(..), showQString, Namespace, QueryString, FormError(..))
import Scrape (runScraperOnHtml)
import Find (findNaive)
import Elem.ChainHTML (contains)
import Elem.SimpleElemParser (el)
import Elem.Types (innerText', ElemHead)
import Links (BaseUrl)

import Test.WebDriver (WD, getSource, runWD, openPage, getCurrentURL, executeJS)
import Test.WebDriver.Commands.Wait (waitUntil, expect, )
import Test.WebDriver.Commands ( Selector(ById, ByXPath), findElem )
import qualified Test.WebDriver.Commands as WD (click)
import Test.WebDriver.Exceptions (InvalidURL(..))
import Test.WebDriver.JSON (ignoreReturn)
import Test.WebDriver.Session (getSession, WDSession)

import Network.HTTP.Types.Header 
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (Manager, Proxy(..), HttpException, httpLbs, responseBody, parseRequest
                           , secure, requestHeaders, newManager, useProxy, managerSetSecureProxy
                           , queryString, path, host, hrFinalRequest, responseOpenHistory, hrFinalResponse
                           , brConsume, brRead, Request, method, Response, CookieJar, responseCookieJar, cookieJar,  HistoriedResponse, BodyReader)
import Network.HTTP.Types.Method (methodGet, Method,)

import System.Directory (removeFile, copyFile, getAccessTime, listDirectory)
import Text.Parsec (ParsecT, Parsec, ParseError, parse, Stream, many)
import Control.Monad.IO.Class (MonadIO, liftIO )
import Control.Monad.Trans.State (StateT, gets, put, get)
import Control.Monad.Trans.Except (ExceptT, )
import Data.Functor.Identity (Identity)
import Control.Exception (Exception, catch)
import Control.Monad.Except (throwError, when)
import Control.Monad.Catch (MonadThrow)
import Data.Maybe (catMaybes)
import Data.Map (Map, toList)
import Data.List (isInfixOf)
import Data.List.Extra (isSuffixOf, maximumBy)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as LazyTX (toStrict, Text)
import qualified Data.Text.Lazy.Encoding as Lazy (decodeUtf8With)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.System (SystemTime)


-- Need a section for Headers logic

type Link = String
type ParsecError = ParseError


type Html = String



-- aside

-- Also need to generalize to MonadIO 

type Url = String 
runScraperOnUrl :: Url -> Parsec Html () a -> IO (Maybe [a])
runScraperOnUrl url p = fmap (runScraperOnHtml p) (getHtml' url)

runScraperOnUrls :: [Url] -> Parsec Html () a -> IO (Maybe [a])
runScraperOnUrls urls p = fmap (foldr (<>) Nothing) $ mapM (flip runScraperOnUrl p) urls 


-- foldr :: (a -> b -> c)

foldFunc :: Maybe [a] -> Maybe [a] -> Maybe [a]
foldFunc = undefined

runScrapersOnUrls = undefined

--- this is meant to be pseudo code at the moment
type STM = IO 

-- | Merge Maybe [a] when multiple urls 
concurrentlyRunScrapersOnUrls :: [Url] -> [ParsecT s u m a] -> STM (Maybe [a])
concurrentlyRunScrapersOnUrls = undefined 
  -- inner will call concurrent stream functions on the given urls 



extractDadBod :: Response ByteString -> String 
extractDadBod response = (unpack . LazyTX.toStrict . mySafeDecoder . responseBody) response

mySafeDecoder :: ByteString -> LazyTX.Text
mySafeDecoder = Lazy.decodeUtf8With (\_ _ -> Just '?')

-- doSignin :: ElemHead -> ElemHead -> Url 
 
-- | Get html with no Proxy 
getHtml' :: Html -> IO Html
getHtml' url = do
  mgrHttps <- newManager tlsManagerSettings
  requ <- parseRequest url
  response <- httpLbs requ mgrHttps
  return $ extractDadBod response
  


-- | Gurantees retrieval of Html by replacing the proxy if we are blocked or the proxy fails 
getHtml :: Manager -> Url -> IO (Manager, Html)
getHtml mgr url = do
  requ <- parseRequest url
  let
    headers = [ (hUserAgent, "Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0")
              , (hAcceptLanguage, "en-US,en;q=0.5")
              , (hAcceptEncoding, "gzip, deflate, br")
              , (hConnection, "keep-alive")
              ]
    req = requ { requestHeaders = (fmap . fmap) (encodeUtf8 . pack) headers
               , secure = True
               }
  (mgr', r) <- catch (fmap ((mgr,) . extractDadBod) $ httpLbs requ mgr) (recoverMgr' url)
  return (mgr', r)


recoverMgr' :: String -> HttpException -> IO (Manager, String)
recoverMgr' url _ = mkProxdManager >>= flip getHtml url


data Clickable = Clickable BaseUrl ElemHead Url deriving (Eq, Show)



-- | Where the sv is effectively constrained to SessionState sv => sv
type SiteM hasSv e a = StateT hasSv (ExceptT e IO) a

-- | TODO: implement default
-- | Where the sv is effectively constrained to SessionState sv => sv
-- type SiteT sv e a = StateT sv (ExceptT e IO) a



-- | Where the sv is effectively constrained to SessionState sv => sv
newtype SiteT sv e a = SiteT { runSite :: StateT sv (ExceptT e IO) a }


-- stepTrajectory :: StateT (deetsWSiteState) IO a
-- stepTrajectory = do
  -- x <- gets siteState
  -- x' <- performSiteState x
  -- putsSiteState 
  
-- mkProxy :: Proxy 
-- mkProxy = Proxy { proxyHost = fst fromScrapeProxies
--                 , proxyPort = snd fromSrapeProxies  
--                 }
type Host = String
type Port = String

-- | All moved to resPapScrap
-- getHtmlSimple :: (HasSessionState s, MonadIO m) => Url -> StateT s m Html
-- getHtmlSimple url = do
--   sv <- gets takeSession
--   (sv', html) <- liftIO $ getHtmlST sv url
--   writeSession sv'
--   pure html
-- getHtmlUrlST :: (HasSessionState s, MonadIO m) => Url -> StateT s m (Html, Url)
-- getHtmlUrlST url = do
--   sv <- gets takeSession
--   (html, url, sv') <- getHtmlAndUrl sv url
--   writeSession sv'
--   pure (html, url)


-- submitFormST :: MonadIO m => FilledForm
--              -> StateT (SiteState sv) m ((Html, Url), FilledForm)
-- submitFormST form = do
--   sv <- gets takeSession
--   ((html, url, sv'), form) <- submitForm sv form
--   writeSession sv'
--   return $ ((html, url), form)

-- clickWritePdfST :: (MonadThrow m, MonadIO m) =>
--                    FilePath
--                 -> Clickable
--                 -> StateT (SiteState sv) m (Either ScrapeException ())
-- clickWritePdfST f c = do
--   -- state <- get
--   -- let
--     -- sv = takeSession state
--   sv <- gets (\state -> takeSession state)
--   eExcSV <- clickWritePdf sv f c
--   -- put $ writeSession s sv
--   case eExcSV of { Left _ -> return (); Right s -> put $ writeSession s sv }
--   pure $ (() <$ eExcSV)

class SessionState a where
  getHtmlST :: (MonadThrow m, MonadIO m) => a -> Url -> m (Html, a)
  getHtmlAndUrl :: (MonadThrow m, MonadIO m) => a -> Url -> m (Html, Url, a)
  submitForm :: (MonadThrow m, MonadIO m) => a -> FilledForm -> m ((Html, Url, a), FilledForm)
  --  Download a pdf link
  clickWritePdf :: (MonadThrow m, MonadIO m) => a -> FilePath -> Clickable -> m (Either ScrapeException a)
  clickWriteFile :: (MonadThrow m, MonadIO m) => a -> FileExtension -> Clickable -> m (Either ScrapeException (), a)
  clickWriteFile' :: a 
                  -> FileExtension -- desired file extension to match
                  -> FilePath -- where to save
                  -> Clickable 
                  -> IO (Either ScrapeException (), a) 





instance SessionState Manager where
  getHtmlST manager url = do
    (m, s) <- liftIO $ getHtml manager url
    return (s, m)

  getHtmlAndUrl manager url = do
    req <- parseRequest url
    liftIO $ catch (baseGetHtml manager req) (saveReq' url getHtmlAndUrl)
  -- getHtmlFlex manager req = catch (baseGetHtml manager req) (saveReq getHtmlFlex req)

  -- Note: qStrVari has data on basic params factored in
  submitForm manager (FilledForm actionUrl reqM term tInput qStrVari) = do
    req <- parseRequest actionUrl
    let
      req2 = req { method = reqM, queryString = (encodeUtf8 . showQString) $ head tInput <> head qStrVari }
      f2 = FilledForm actionUrl reqM term tInput (tail qStrVari)
    liftIO $ fmap (, f2) $ catch (baseGetHtml manager req2) (saveReq req2 baseGetHtml)

  clickWritePdf manager filepath x@(Clickable baseU _ url) = do
    (pdf, mgr) <- getHtmlST manager url
    -- path <- liftIO $ resultPath searchTerm (getHost baseU) (Paper x) >>= flip writeFile pdf
    liftIO $ writeFile filepath pdf
    return $ Right mgr
      -- Invalidate normal HTML responses here------
      -- AND if file did not download then this was not a PdfLink like expected



-- we only actually wanna do this in the case of
-- getHtmlST' :: (HasSessionState hasSv, MonadIO m) => Url -> StateT hasSv m Html
-- getHtmlST' = do
--   (c, m) <- gets cookies <*> gets manager
--   url <- getHtmlST (c,m) url
--   return ""

-- getHtmlReq :: CookieManager -> Request -> IO (CookieManager, Html)
-- getHtmlReq mgr req = do
--   fmap (mgr,) $ httpLbs req mgr -- ?

data CookieManager = CookieManager CookieJar Manager

-- getHtmlST :: Url || Form ||

--- - MonadIO m, HasSessionState s) => SiteT s m a
 
-- we dont fucking need this, just take idea with seshVar (get, put set up)
-- class HasSessionState a sv | a -> sv where
--   takeSession :: SessionState sv => a -> sv
--   writeSession :: SessionState sv => sv -> a -> a
                    
setCJ :: CookieJar -> Request -> Request
setCJ cj req = req { cookieJar = Just cj }


buildReq :: MonadThrow m => CookieJar -> Url -> m Request
buildReq cj url = setCJ cj <$> parseRequest url



-- | Gurantees retrieval of Html by replacing the proxy if we are blocked or the proxy fails
getHtmlMgr :: Manager -> Url -> IO (Manager, Html)
getHtmlMgr mgr url = do
  requ <- parseRequest url
  let
    headers = [ (hUserAgent, "Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0")
              , (hAcceptLanguage, "en-US,en;q=0.5")
              , (hAcceptEncoding, "gzip, deflate, br")
              , (hConnection, "keep-alive")
              ]
    req = requ { requestHeaders = (fmap . fmap) (encodeUtf8 . pack) headers
               , secure = True
               }
  (mgr', r) <- catch (fmap ((mgr,) . extractDadBod) $ httpLbs requ mgr) (recoverMgr' url)
  return (mgr', r)


{-# DEPRECATED baseGetHtml "needs extractDadBod" #-}
baseGetHtml :: Manager -> Request -> IO (Html, Url, Manager)
-- baseGetHtml Request -> ReaderT Manager IO (Html, Url)
baseGetHtml manager req = do
  hResponse <- responseOpenHistory req manager
  let
    finReq = hrFinalRequest hResponse
    dadBodNew response = (unpack . decodeUtf8) response
  finResBody <- brRead $ responseBody $ hrFinalResponse hResponse
  return (dadBodNew finResBody, (unpack . decodeUtf8) $ (host finReq) <> (path finReq) <> (queryString finReq), manager)

-- | Gurantees retrieval of Html by replacing the proxy if we are blocked or the proxy fails
getHtmlMgr' :: Manager -> Url -> IO (Manager, Html)
getHtmlMgr' mgr url = do
  let
    headers = [ (hUserAgent, "Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0")
              , (hAcceptLanguage, "en-US,en;q=0.5")
              , (hAcceptEncoding, "gzip, deflate, br")
              , (hConnection, "keep-alive")
              ]
  getHtmlHeaderMgr headers mgr url

  -- return (mgr', r)

getHtmlHeaderMgr :: [Header] -> Manager -> Url -> IO (Manager, Html)
getHtmlHeaderMgr headers mgr url = do
  -- (fmap.fmap) extractDadBod . $
  (mgr, res) <- persistGet mgr =<< mkReq headers url
  res' <- getHistoriedBody $ hrFinalResponse res
  pure (mgr, res')
  

mkReq :: MonadThrow m => [Header] -> Url -> m Request
mkReq headers url = fmap (setHeaders headers) $ parseRequest url
  where setHeaders headers req = req { requestHeaders = headers }
  -- return (mgr', r)

recoverMgr :: Request
           -> HttpException
           -> IO (Manager, HistoriedResponse BodyReader)
recoverMgr req _ = flip persistGet req =<< mkProxdManager

persistGet :: MonadIO m => Manager
           -> Request
           -> m (Manager, HistoriedResponse BodyReader)
persistGet sv req = liftIO $ catch (getHtmlHistoried sv req) (recoverMgr req)

-- | Only applies sv into http functions
getHtmlHistoried :: MonadIO m => Manager
                -> Request
                -> m (Manager, HistoriedResponse BodyReader)
getHtmlHistoried m r = liftIO $ fmap (m,) $ responseOpenHistory r m

  
    --getHtmlFlex manager req = catch (baseGetHtml manager req) (saveReq getHtmlFlex req)


  -- getHtm
  -- getHtmlST url = do
  --   CookieManager cj mgr <- gets sv
  --   req <- parseRequest url
  --   (m, s) <- getHtmlReq mgr $ setCJ cj req
  --   return (s, m)

-- | this fails if its a post request
requestToUrl' :: Request -> Maybe Url
requestToUrl' req = if methodGet == method req
                    then Just . unpack . decodeUtf8 $ (host req) <> (path req) <> (queryString req)
                    else Nothing

-- works for any request
getURL :: Request -> Url
getURL req =  unpack . decodeUtf8 $ (host req) <> (path req) <> (queryString req)


drop1qStrVar :: FilledForm -> FilledForm
drop1qStrVar (FilledForm a b c d qStr) = FilledForm a b c d (drop 1 qStr)

mkFormRequest :: MonadThrow m => Url -> Method -> QueryString -> m Request
mkFormRequest url reqMethod qString = do
  req <- parseRequest url
  pure $ req { method = reqMethod
             , queryString = encodeUtf8 . showQString $ qString
             }

getHistoriedBody :: Response BodyReader -> IO Html 
getHistoriedBody res = fmap (readHtml . fromStrict) $ brRead . responseBody $ res


-- extractDadBod :: Response ByteString -> String 
-- extractDadBod response = (unpack . LazyTX.toStrict . mySafeDecoder . responseBody) response

 
-- mySafeDecoder :: ByteString -> LazyTX.Text
-- mySafeDecoder = Lazy.decodeUtf8With (\_ _ -> Just '?')


readHtml :: ByteString -> Html
readHtml = unpack . LazyTX.toStrict . mySafeDecoder

instance SessionState CookieManager where
  getHtmlST cm@(CookieManager cj mgr) url = do
    req <- buildReq cj url
    (manager, response) <- persistGet mgr req
    let
      finalResponse = hrFinalResponse response
      
      newCookies = responseCookieJar finalResponse
    html <- liftIO $ getHistoriedBody finalResponse
    return (html, CookieManager (cj <> newCookies) manager)

  getHtmlAndUrl (CookieManager cj mgr) url = do
    req <- buildReq cj url
    (manager, response) <- persistGet mgr req
    let
      finalResponse = hrFinalResponse response
      
      newCookies = responseCookieJar finalResponse
      lastUrl = getURL $ hrFinalRequest response
    html <- liftIO $ getHistoriedBody finalResponse
    return (html, lastUrl, CookieManager (cj <> newCookies) manager)
    
    -- (html, ) getHtmlST cm url 
    -- req <- parseRequest url
    -- catch (baseGetHtml manager req) (saveReq' url getHtmlAndUrl)

  submitForm (CookieManager cj mgr) form@(FilledForm actionUrl reqM term tInput qStrVari) = do
    req <- mkFormRequest actionUrl reqM (head tInput <> head qStrVari)
    (manager, response) <- persistGet mgr req
    let
      finalResponse = hrFinalResponse response
      
      newCookies = responseCookieJar finalResponse
    -- undefined cuz it needs to be removed --> this is never used here
    html <- liftIO $ getHistoriedBody finalResponse
    pure ((html, undefined, CookieManager (cj <> newCookies) manager), drop1qStrVar form)

  clickWritePdf cmanager filepath x@(Clickable baseU _ url) = do
    (pdf, mgr) <- getHtmlST cmanager url
    liftIO $ writeFile filepath pdf
    return $ Right mgr
 
    -- (html, cm) <- getHtmlST cmanager req 
    -- pure ((html, cm), drop1qStrVar form)
 
  -- -- Note: qStrVari has data on basic params factored in
  -- submitForm manager (FilledForm actionUrl reqM term tInput qStrVari) = do
  --   req <- parseRequest actionUrl
  --   let
  --     req2 = req { method = reqM
  --                , queryString = (encodeUtf8 . showQString)
  --                                $ head tInput <> head qStrVari }
  --     formToDo = FilledForm actionUrl reqM term tInput (tail qStrVari)
  --   fmap (, formToDo) $ catch (baseGetHtml manager req2) (saveReq req2 baseGetHtml)
 
  -- clickWritePdf cmanager filepath x@(Clickable baseU _ url) = do
  --   (pdf, mgr) <- getHtmlST cmanager url
  --   -- path <- liftIO $ resultPath searchTerm (getHost baseU) (Paper x) >>= flip writeFile pdf 
  --   writeFile filepath pdf
  --   return $ Right mgr
      -- Invalidate HTML responses here------
      -- AND if file did not download then this was not a PdfLink like expected




instance SessionState WDSession where
  getHtmlST = getHtmlWD -- use runWD to run a WD action where you do
                                         -- 1) Get html (getSource)
                                         -- 2) tuple with result of getSession

  getHtmlAndUrl = getHtmlUWD




                                                                             --(qStr:qStrs)
  -- submitForm wdSesh (FilledForm actionUrl reqMethod' searchTerm' tio []) =
  submitForm wdSesh (FilledForm actionUrl reqMethod' searchTerm' tio (qStr:qStrs)) = do
    liftIO $ print "inside submit form"
    liftIO $ print "Action URL:"
    liftIO $ print actionUrl
    if reqMethod' == methodGet
      then
      do
        liftIO $ print "do get"
        -- write Url then fetch it
        truple@(html, url, wdSesh') <- liftIO $ runWD wdSesh (wdSubmitFormGET actionUrl (head tio <> qStr))
        return (truple, FilledForm actionUrl reqMethod' searchTerm' tio qStrs) --qStrs)
      else
      do
        liftIO $ print "do post"
        -- write form elem with static (namespace,value) then hit submit
        truple@(html,url,wdSesh') <- liftIO $ runWD wdSesh (submitPostFormWD $ writeForm (pack actionUrl) (head tio <> qStr))
        return (truple, FilledForm actionUrl reqMethod' searchTerm' tio qStrs) --qStrs)


  -- clickWritePdf wdSesh (Clickable baseU (e, attrs) url) =
  --   if isSuffixOf ".pdf" url
  --   then
  --     do
  --       (pdf, wdSesh') <- getHtmlST wdSesh url
  --       liftIO $ writeFile (resultFolder baseU Pdf) pdf
  --       return (Right wdSesh')

  --   else
  --     runWD wdSesh $ do
  --     e <- findElem (ByXPath . pack $ xpath (e, attrs))
  --     WD.click e
  --     -- src <- waitUntil 10 (do
  --                             -- | SHOULD CHANGE TO checking if .pdf format
  --                             -- | thats a lot of work tho sooooo....
  --                             -- src <- getSource
  --                             -- expect (if ((length (unpack src)) < 10000) then False else True)
  --                             -- return src
  --                         -- )
  --     pdf <- lift $ takeNewestFile baseU
  --     let
  --       host baseU = undefined
  --     liftIO $ resultPath searchTerm baseU->host (Paper x) >>= flip writeFile pdf

  --     fmap Right getSession
  --     -- (unpack src,) <$> getSession




  -- | comment is to crash nix as reminder to move somewhere sensible
-- data OpenStruct a = OpenStruct (Parser a)
-- type CloseStruct a = OpenStruct a -> ClosePiece a
  -- could be even \_ -> f , when the Close struct is independent of Open and I dont think this
  -- would affect speed
-- data ClosePiece a = ClosePiece (Parser a)  

  

-- Could eventually open this up to further extensions
-- If something is a tree like string structure then we could extend to MessyTreeMatch which is
-- configurable to Open and Close 
type FileExtension = FilePath

data ScrapeException = --NoAdvSearchJustBasic
  NoSearchOnSite
  | CantDerivePagination
  | InvalidPaginate
  | ItemResultNF
  | PDFRequestFailed
  | NoSearchItems
  | AuthError
  | FormErr FormError
  | OtherError String
  | PromisedPdfDownloadNF String
  deriving Show

instance Exception ScrapeException


-- instance Trajectory s => Trajectory SiteT s m a where

-- | A trajectory is a self contained, possibly recursive scraping plan
-- | Each state is simply a message to the runner, where we left off
-- |
-- | Note too that we could use this for ResearchGate
-- |
-- | data ResearchGate = PdfPage _x_ | RelatedToPage _y_ | GetPdf? | other
-- | and the site has a recursive layout / papers do so this can just infinitely recurse between the opts
-- | we could also institute some runner Function that runs the trajectory some given N times 
class Eq s => Trajectory s where
  end :: s
  -- could also call this stepTrajectory
  performSiteState :: MonadIO m => s -> m s


-- -- | As long as we have

-- -- ...
-- end :: MySiteState
-- end = SiteEndState



-- could also be called manageTrajectories
performSiteStateMultiple :: (MonadIO m, Trajectory s) => [s] -> m ()
performSiteStateMultiple trajs = do
  let
    tr' = dropWhile (==end) trajs
  newState <- performSiteState (head tr')
  when (not . null $ tr') $ performSiteStateMultiple $ (tail tr') <> (newState:[])

  --when (s == end) $ performSiteStateMultiple ss


performSiteStateSingle :: (Trajectory s, MonadIO m) => s -> m ()
performSiteStateSingle s = do
  if (s == end)
    then return ()
    else performSiteState s >>= performSiteStateSingle





saveReq :: Request
        -> (Manager -> Request -> IO (Html, Url, Manager))
        -> HttpException
        -> IO (Html, Url, Manager)
saveReq req func _ = do
  newManager <- mkProxdManager
  func newManager req


saveReq' :: Url
        -> (Manager -> Url -> IO (Html, Url, Manager))
        -> HttpException
        -> IO (Html, Url, Manager)
saveReq' req func _ = do
  newManager <- mkProxdManager
  func newManager req

    -- hrFinalRequest res

    -- mgr <- newManager tlsManagerSettings
    -- res <- fmap (responseBody . hrFinalResponse) (responseOpenHistory req mgr) >>= brRead

    -- let
      -- finalDraftResponse = dadBod res

    -- let
    --   f :: Manager -> Request -> IO (Html, Url, Manager)
    --   f mgr req = do
    --     hResponse <- responseOpenHistory req mgr
    --     -- res <- hrFinalResponse hRes
    --     -- res <- (brConsume $ hrFinalResponse hRes)
    --     let
    --       finReq = hrFinalRequest hResponse
    --       dadBodNew response = (unpack . decodeUtf8) response
    --     finResBody <- (brRead (responseBody $ hrFinalResponse hResponse))
    --     return (dadBodNew finResBody, (unpack . decodeUtf8) $ (host finReq) <> (path finReq) <> (queryString finReq), mgr)

------------------------------------------------------------------------------------------------------------------
    -- fmap (responseBody . hrFinalResponse) (responseOpenHistory req mgr) >>= brRead
------------------------------------------------------------------------------------------------------------------



type DownloadsFolder = FilePath

-- | Pulls newest file in downloads folder into program/IO scope 
takeNewestFile :: DownloadsFolder -> Clickable -> BaseUrl -> ExceptT ScrapeException IO String
takeNewestFile dwnlds clickable baseU = do
  dirNames <- liftIO $ listDirectory dwnlds
  when (length dirNames == 0) $ throwError (PromisedPdfDownloadNF
                                        "either link was not a pdf or did not download properly or not yet")
  times <- liftIO $ mapM getAccessTime dirNames
  let
    f :: [(FilePath, UTCTime)]
    f = zip dirNames times
    newFile = maximumBy (\x y -> if snd x > snd y then GT else LT) f
  ---------------------------------------------
  --Invalidate normal HTML responses here------
  -- AND if file did not download then this was not a PdfLink like expected
  ---------------------------------------------
  -- liftIO $ copyFile (fst newFile) filepath 
  liftIO $ readFile (fst newFile) <* (removeFile $ fst newFile)
  -- return pdf



xpath :: ElemHead -> String
xpath (e, attrs) = "//" <> e <> (attrsXpath attrs)

attrsXpath :: Map String String -> String
attrsXpath m =
  let
    lis = toList m
    f :: (String, String) -> String
    f (k,v) = "[@" <> k <> "='" <> v <> "']"
  in
    mconcat $ fmap f lis

-- type ActionUrl = Url
-- data Form' = Form' SearchTerm ActionUrl Method QueryString [QueryString]

  -- submitForm wdSesh (FilledForm{..}) = do
    -- liftIO $ print "inside submit form"
    -- let
      -- f2 = FilledForm baseUrl reqMethod searchTerm actnAttr textInputOpts (tail qStringVariants)
    -- if (reqMethod == methodGet)
      -- then fmap (,f2) $ runWD wdSesh (wdFuncF baseUrl actnAttr textInputOpts qStringVariants)
      -- else fmap (,f2) $ postFormWD wdSesh (FilledForm baseUrl reqMethod searchTerm actnAttr textInputOpts qStringVariants)
    -- where
      -- wdFuncF baseU aAttr tio qStrVari = do
        -- openPage $ baseU <> "/" <> (unpack $ aAttr <> (showQString $ (head tio) <> (head qStrVari)))
        -- (,,) <$> (fmap unpack getSource) <*> getCurrentURL <*> getSession

wdSubmitFormGET :: Url -> QueryString -> WD (String, Url, WDSession)
wdSubmitFormGET actionUrl tioqStrVari = do
  openPage (actionUrl <> "?" <> (unpack (showQString $ tioqStrVari)))
  src <- waitUntil 10 (do
                          src <- getSource
                          expect (if ((length (unpack src)) < 50000) then False else True)
                          return $ unpack src
                      )
  (,,) <$> (fmap unpack getSource) <*> getCurrentURL <*> getSession


-- postFormWD :: WDSession -> FilledForm -> IO (Html, Url, WDSession)
-- postFormWD sesh form =
  -- (liftIO $ putStrLn "writing and submitting POST form")
  -- >> runWD sesh (submitPostFormWD $ writeForm form)

-- FilledForm baseUrl' reqMethod' searchTerm' actnAttr' textInputOpts' (qStr:qStrs)
-- | actnAttr is part of Url
writeForm :: Text -> QueryString -> Text
writeForm url qString =
  "<form id=\"myBullshitForm\""
  <> " method=\"post\""
  <> " action=\"" <> url <> "\""
  <> ">"
  -- <> ""
  <> "hey"
  <> writeFormParams qString
  <> "</form>"


-- | actnAttr is part of Url
-- writeForm' :: Url -> Method -> QueryString -> Text
-- writeForm' (FilledForm {..}) =
  -- "<form id=\"myBullshitForm\""
  -- <> " method=\"" <> (if reqMethod == methodGet then "get" else "post") <> "\""
  -- <> " action=\"" <> (pack baseUrl <> "/" <> actnAttr) <> "\""
  -- <> ">"
  -- <> writeFormParams (head textInputOpts <> head qStringVariants)
  -- <> "</form>"





writeFormParams :: QueryString -> Text
writeFormParams [] = ""
writeFormParams (param:params) = writeParam param <> writeFormParams params

writeParam :: (Namespace, Text) -> Text
writeParam (n, v) =
  "<input type=\"hidden\""
  <> " name=\"" <> n <> "\""
  <> " value=\"" <> v <> "\""
  <> ">"

submitPostFormWD :: Text -> WD (Html, Url, WDSession)
submitPostFormWD formString = do
  -- liftIO $ print "the magic expression is!!!"
  -- liftIO $ print formString
  ignoreReturn $ executeJS [] ("document.body.innerHTML = '" <> formString <> "'") --formString)
  -- ignoreReturn $ executeJS [] formString

  -- liftIO $ print "expr 1 of 4 run"
  ignoreReturn $ executeJS [] "console.log('hello')"
  -- liftIO $ print "expr 2 of 4 run"
  ignoreReturn $ executeJS [] "document.getElementById('myBullshitForm').submit()"

  src <- waitUntil 10
         (do { src <- getSource; expect (if ((length (unpack src)) < 50000) then False else True); return src })
  (unpack src,,) <$> getCurrentURL <*> getSession

  -- getHtmlFlex manager req = getHtmlFlexWd

-- getHtmlFlexWd :: WDSession -> Request -> IO (Html, Url, WDSession)
-- getHtmlFlexWd seshVar req = do
--   runWD seshVar f



-- f :: Request -> WD (Html, Url, WDSession)
-- f req = do
--   eReq <- sendHttpRequest req
--   case eReq of
--     Right res -> ""
--     Left _ -> ""

getHtmlWD :: MonadIO m => WDSession -> Url -> m (Html, WDSession)
getHtmlWD seshVar url = liftIO $ runWD seshVar (wd url)
  where
    wd :: Url -> WD (Html, WDSession)
    wd urlI = do
      openPage urlI
      src <- waitUntil 10 (do
                              src <- getSource
                              expect (if ((length (unpack src)) < 10000) then False else True)
                              return src
                          )
      (unpack src,) <$> getSession
    -- where
      -- f = src <- waitUntil 10 (do
                                  -- src <- getSource
                                  -- expect (if ((length (unpack src)) < 2000) then False else True)
                                  -- return src
                              -- )
getHtmlUWD :: MonadIO m => WDSession -> Url -> m (Html, Url, WDSession)
getHtmlUWD sv url = liftIO $ runWD sv (f_ url)

  -- (,,) <$> getCurrentUrl <*> getSource <*> getSession

f_ :: Url -> WD (Html, Url, WDSession)
f_ url = do
  -- openPage url
  openPage url
  src <- waitUntil 10
         (do { src <- getSource; expect (if ((length (unpack src)) < 10000) then False else True); return src })
  (unpack src,,) <$>  getCurrentURL <*> getSession




-- saveReqForm req = do
  -- manager <- mkProxdManager
  -- baseGetHtml

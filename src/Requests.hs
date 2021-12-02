{-# LANGUAGE FlexibleContexts #-}

module Requests where 

import Find (findNaive)
import Scrape (runScraperOnHtml)
import Elem.SimpleElemParser (el)
import Elem.Types (innerText')
import Elem.ChainHTML (contains)

import Data.Maybe (catMaybes)
import Data.List (isInfixOf)
import Network.HTTP.Types.Header 
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT, Parsec, ParseError, parse, Stream, many)
import Network.HTTP.Client (Manager, Proxy(..), HttpException, httpLbs, responseBody, parseRequest
                           , secure, requestHeaders, newManager, useProxy, managerSetSecureProxy)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Exception (catch)

type Link = String
type ParsecError = ParseError


-- Applied inside of execState                          ---Goal---IO (Either ParsecError (Maybe a))
scrapeUrlWith :: ParsecT Text () Identity a -> Manager -> Link -> IO (Either ParsecError a)
scrapeUrlWith parser manager url = do
  --replace with successive requesting with cookies
  -- let url' = evalLink url
  request <- parseRequest url
  response <- httpLbs request manager
  let
    dadBod = toStrict $ decodeUtf8 (responseBody response)
  -- response <- (decodeUtf8 (responseBody response)) <$> httpLbs request manager

  return $ parse parser ("site" <> url) dadBod


type Html = String



-- aside

-- Also need to generalize to MonadIO 

type Url = String 
runScraperOnUrl :: Url -> Parsec String () a -> IO (Maybe [a])
runScraperOnUrl url p = fmap (runScraperOnHtml p) (getHtml' url)

runScraperOnUrls :: [Url] -> Parsec String () a -> IO (Maybe [a])
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




-- | Get html with no Proxy 
getHtml' :: String -> IO String
getHtml' url = do
  mgrHttps <- newManager tlsManagerSettings
  requ <- parseRequest url
  response <- httpLbs requ mgrHttps
  let
    dadBod = (unpack . toStrict . decodeUtf8 . responseBody) response

  return dadBod


-- | Gurantees retrieval of Html by replacing the proxy if we are blocked or the proxy fails 
getHtml :: Manager -> String -> IO (Manager, Html)
getHtml manager url = do
  -- mgrHttps <- newManager tlsManagerSettings
  requ <- parseRequest url
  let
    headers = [ (hUserAgent, "Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0")
              -- , (hAccept, "image/webp, */*")
              , (hAcceptLanguage, "en-US,en;q=0.5")
              , (hAcceptEncoding, "gzip, deflate, br")
              , (hConnection, "keep-alive")
              -- , (hReferer, "https://www.amazon.ca/")
              -- , (hTE, "Trailers")
              ]
    req = requ { requestHeaders = (fmap . fmap) (encodeUtf8 . pack) headers
               , secure = True
               }

    dadBod response = (unpack . toStrict . decodeUtf8 . responseBody) response
    
    f = do
      res <- httpLbs requ manager
      return (manager, dadBod res)

    g :: HttpException -> IO (Manager, String)
    g = (\_ -> do
            newManager <- mkManager
            getHtml newManager url
        )
  (manager', response) <- catch f g 
  return (manager', response)




testForValidProxy :: [Proxy] -> IO (Manager)
testForValidProxy (proxy:proxies) = do
  req <- parseRequest "https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#v:catch"
  trialManager <- mkManagerInternal proxy
  print $ "hello"
  let
    f :: IO (Manager)
    f = catch (httpLbs req trialManager >> return trialManager) g

    g :: HttpException -> IO (Manager)
    g = (\_ -> testForValidProxy proxies) 
  x <- f
  return x



rowToProxy :: [String] -> Proxy
rowToProxy row = Proxy ((encodeUtf8 . pack) (row !! 0)) (read (row !! 1) :: Int)

mkManagerInternal :: Proxy -> IO Manager
mkManagerInternal proxy = newManager (managerSetSecureProxy (useProxy proxy) tlsManagerSettings)

  

mkManager :: IO (Manager)
mkManager = do
  proxyRows <- scrapeProxyList
  testForValidProxy proxyRows




scrapeProxyList :: IO [Proxy] -- becoming [[String]]
scrapeProxyList = do
  response <- getHtml' "https://free-proxy-list.net/"
  let
    parser = el "tr" [] `contains` b
    b :: Stream s m Char => ParsecT s u m [String]
    b = ((fmap . fmap) innerText' $ many (el "td" []))

    bePicky :: [[String]] -> [[String]]
    bePicky rows = filter (\x -> not $ (isInfixOf "anonymous" (x !! 4)) || (isInfixOf "elite proxy" (x !! 4))) rows
    
    -- g :: [String] -> Proxy
    -- g row = Proxy ((encodeUtf8 . pack) (row !! 0)) (read (row !! 1) :: Int)
    
  -- mapM_ print $ fromMaybe [] $ fromRight Nothing (parse (findNaive parser) "" response)
    
  case parse (findNaive parser) "" response of
    Right (Just (rows)) -> --f rows   --row is a list of 9ish "td" (cell:rowCells)
      return (fmap rowToProxy (rows))
      --  $ (\row -> Proxy (row !! 0) (row !! 1))
    Right (Nothing) -> ioError $ userError "proxy error: couldnt get proxy"
    Left err -> ioError $ userError (show err)
    


type SiteM sv a = StateT (SiteDetails sv) (ExceptT ScrapeException' IO) a

-- | Where the sv is effectively constrained to SessionState sv => sv
type SiteM sv a = StateT sv (ExceptT ScrapeException' IO) a

-- | TODO: implement default
-- | Where the sv is effectively constrained to SessionState sv => sv
type SiteT sv e a = StateT sv (ExceptT e IO) a



-- | Where the sv is effectively constrained to SessionState sv => sv
newtype SiteM sv e a = SiteM { runSite :: StateT sv (ExceptT e IO) a }



-- mkProxy :: Proxy 
-- mkProxy = Proxy { proxyHost = fst fromScrapeProxies
--                 , proxyPort = snd fromSrapeProxies  
--                 }
type Host = String
type Port = String


class SessionState a where
  getHtmlST :: a -> Url -> IO (Html, a)
  getHtmlAndUrl :: a -> Url -> IO (Html, Url, a)
  submitForm :: a -> FilledForm -> IO ((Html, Url, a), FilledForm)
  clickWritePdf :: a -> Clickable -> IO (Either ScrapeException a) -- | Download a pdf link
  -- clickPdf' :: a -> Clickable -> IO (Either ScrapeException (Html, a))
    -- even if it goes to the downloads folder, we just read the file as Pdf text or even Maybe Pdf text like below
  -- clickPdf'' :: a -> Clickable -> IO (Either ScrapeException (Maybe Pdf, a))
    -- because we should be applying some checks to ensure this is not a mistake
    -- Might be able to simply search for indicators of html and if none found then we assume it could
    -- only be a Pdf
    -- additionally we should return our SessionState


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


class Trajectory s where
  end :: s
  -- could also call this stepTrajectory
  performSiteState :: (MonadIO m, Trajectory s) => s -> m ()


-- | As long as we have
end :: SiteState s => s
...
end :: MySiteState
end = SiteEndState

-- could also be called manageTrajectories
performSiteStateMultiple :: (MonadIO m, Trajectory s) => [s] -> m ()
performSiteStateMultiple trajs = do
  let
    tr' = dropWhile (==end) trajs
  newState <- performSiteState (head tr')
  performSiteStateMultiple $ (tail tr') <> (newState:[])

  --when (s == end) $ performSiteStateMultiple ss


performSiteStateSingle :: MonadIO m => s -> m ()
performSiteStateSingle s = do
  if (s == end)
    then return ()
    else performSiteState s >>= performSiteStateSingle





saveReq :: Request
        -> (Manager -> Request -> IO (Html, Url, Manager))
        -> HttpException
        -> IO (Html, Url, Manager)
saveReq req func _ = do
  newManager <- mkManager
  func newManager req


saveReq' :: Url
        -> (Manager -> Url -> IO (Html, Url, Manager))
        -> HttpException
        -> IO (Html, Url, Manager)
saveReq' req func _ = do
  newManager <- mkManager
  func newManager req


baseGetHtml :: Manager -> Request -> IO (Html, Url, Manager)
baseGetHtml manager req = do
  hResponse <- responseOpenHistory req manager
  let
    finReq = hrFinalRequest hResponse
    dadBodNew response = (unpack . TE.decodeUtf8) response
  finResBody <- (brRead (responseBody $ hrFinalResponse hResponse))
  return (dadBodNew finResBody, (unpack . TE.decodeUtf8) $ (host finReq) <> (path finReq) <> (queryString finReq), manager)

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
    --       dadBodNew response = (unpack . TE.decodeUtf8) response
    --     finResBody <- (brRead (responseBody $ hrFinalResponse hResponse))
    --     return (dadBodNew finResBody, (unpack . TE.decodeUtf8) $ (host finReq) <> (path finReq) <> (queryString finReq), mgr)

------------------------------------------------------------------------------------------------------------------
    -- fmap (responseBody . hrFinalResponse) (responseOpenHistory req mgr) >>= brRead
------------------------------------------------------------------------------------------------------------------



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
        truple@(html, url, wdSesh') <- runWD wdSesh (wdSubmitFormGET actionUrl (head tio <> qStr))
        return (truple, FilledForm actionUrl reqMethod' searchTerm' tio qStrs) --qStrs)
      else
      do
        liftIO $ print "do post"
        -- write form elem with static (namespace,value) then hit submit
        truple@(html,url,wdSesh') <- runWD wdSesh (submitPostFormWD $ writeForm (pack actionUrl) (head tio <> qStr))
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

takeNewestFile :: Genre -> Clickable -> BaseUrl -> ExceptT ScrapeException IO String
takeNewestFile genre clickable baseU = do
  dirNames <- liftIO $ listDirectory "home/lazylambda/scrappyDownloads"
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
  liftIO $ copyFile (fst newFile) (resultPath genre baseU (Paper clickable))
  pdf <- readFile (fst newFile)
  liftIO $ removeFile $ fst newFile
  return pdf

resultFolder :: BaseUrl -> Item -> FilePath
resultFolder b res =
  let
    res' = case res of
             Pdf -> "/papers"
             WrAbstract -> "/abstract"
  in
    "home/lazylambda/code/Ace/resPapScrap/Results/" <> b <> res'



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
  <> "</form>"


getHtmlStateful :: (MonadIO m, SessionState sv) => String -> StateT (SiteDetails sv) m Html
getHtmlStateful url = do
  s <- gets seshVar
  (html, s') <- liftIO $ getHtmlST s url
  putsSVar s'
  return html


putsSVar :: (Monad m, SessionState a) => a -> StateT (SiteDetails a) m ()
putsSVar sVar = do
  state1 <- get
  let
    state2 = state1 { seshVar = sVar }
  put state2



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

getHtmlWD :: WDSession -> Url -> IO (Html, WDSession)
getHtmlWD seshVar url = do
  runWD seshVar (wd url)
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
getHtmlUWD :: WDSession -> Url -> IO (Html, Url, WDSession)
getHtmlUWD sv url = do
  runWD sv (f_ url)
  -- (,,) <$> getCurrentUrl <*> getSource <*> getSession

f_ :: Url -> WD (Html, Url, WDSession)
f_ url = do
  -- openPage url
  openPage url
  src <- waitUntil 10
         (do { src <- getSource; expect (if ((length (unpack src)) < 10000) then False else True); return src })
  (unpack src,,) <$>  getCurrentURL <*> getSession



instance SessionState Manager where
  getHtmlST manager url = do
    (m, s) <- getHtml manager url
    return (s, m)

  getHtmlAndUrl manager url = do
    req <- parseRequest url
    catch (baseGetHtml manager req) (saveReq' url getHtmlAndUrl)
  -- getHtmlFlex manager req = catch (baseGetHtml manager req) (saveReq getHtmlFlex req)

  -- Note: qStrVari has data on basic params factored in
  submitForm manager (FilledForm actionUrl reqM term tInput qStrVari) = do
    req <- parseRequest actionUrl
    let
      req2 = req { method = reqM, queryString = (encodeUtf8 . showQString) $ head tInput <> head qStrVari }
      f2 = FilledForm actionUrl reqM term tInput (tail qStrVari)
    fmap (, f2) $ catch (baseGetHtml manager req2) (saveReq req2 baseGetHtml)

  clickWritePdf manager searchTerm x@(Clickable baseU _ url) = do
    (pdf, mgr) <- getHtmlST manager url
    path <- liftIO $ resultPath searchTerm baseU->host (Paper x) >>= flip writeFile pdf
    -- writeFile path html
    return $ Right mgr

      Invalidate normal HTML responses here------
      AND if file did not download then this was not a PdfLink like expected



-- saveReqForm req = do
  -- manager <- mkManager
  -- baseGetHtml

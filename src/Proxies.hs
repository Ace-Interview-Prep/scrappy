{-# LANGUAGE FlexibleContexts #-}

module Proxies where

import Scrape (runScraperOnHtml)
import Elem.ChainHTML (contains)
import Elem.SimpleElemParser (el)
import Elem.Types (innerText')

import Text.Parsec (ParsecT, Stream, many)
import Network.HTTP.Client (Proxy, Manager, HttpException, Response, Proxy(..), responseBody, httpLbs
                           , parseRequest, newManager, useProxy, managerSetSecureProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Exception (catch)
import Data.List (isInfixOf)
import Data.Text (unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LazyTX (toStrict, Text)
import qualified Data.Text.Lazy.Encoding as Lazy (decodeUtf8With)
import Data.ByteString.Lazy (ByteString)



 
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

  

mkProxdManager :: IO (Manager)
mkProxdManager = do
  proxyRows <- scrapeProxyList
  testForValidProxy proxyRows



-- | Table pattern should also take a Cell where data Cell = (X,Y)
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
    
  case runScraperOnHtml parser response of
    Just (rows) ->
      return (fmap rowToProxy (rows))
    Nothing -> ioError $ userError "proxy error: couldnt get proxy"
    
    
------
--Move to Internal module:https://stackoverflow.com/questions/8650297/haskell-recursive-circular-module-definitions
------

 
-- | Get html with no Proxy 
getHtml' :: String -> IO String
getHtml' url = do
  mgrHttps <- newManager tlsManagerSettings
  requ <- parseRequest url
  response <- httpLbs requ mgrHttps
  return $ extractDadBod response



extractDadBod :: Response ByteString -> String 
extractDadBod response = (unpack . LazyTX.toStrict . mySafeDecoder . responseBody) response

mySafeDecoder :: ByteString -> LazyTX.Text
mySafeDecoder = decodeUtf8Withith.Lazy (\_ _ -> Just '?')

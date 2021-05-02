{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test where

-- cuz im too dang lazy to set up a test suite yet

-- import Text.RawString.QQ (r)

import Elem.Types 
import Elem.ElemHeadParse 
import Text.Parsec (parse, ParsecT, Stream, many, char, string, (<|>), anyChar, manyTill, ParseError)
import Elem.SimpleElemParser
import Find


import qualified Data.Map as Map

import Links
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Network.HTTP.Client
import Network.HTTP.Client.TLS 
import Data.List.Extra (isInfixOf)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Text (pack, unpack, Text)
import Data.Functor.Identity (Identity)
-- | PASSED:

  --parseOpeningTag


-- | FAILS:

  -- > ?? isAttrsMatch: *** Exception: src/Elem/ElemHeadParse.hs:(119,1)-(122,97): Non-exhaustive patterns in function isAttrsMatch
  
  -- >  innerElemParsers : all of them need to replace 


-- | Notes

  -- manyTill_ over manyTill , want to consume that output
  
  -- parserZero should maybe be parserFail "message" 

  -- &amp may be purposeful seperator for text






-------------------------------------------------------------------------------------------------------------------




-- t1 = [r|<a href="re">red</a>|]

-- testStr = [r|"hey"|]

-- parser = parseOpeningTag (Just ["a"]) [("href", Just "red")] 
-- main1 = parse parser "" t1

-- parser2 :: Stream s m Char => ParsecT s u m (Elem' String)
-- parser2 = elemParser (Just ["a"]) Nothing []


-- t2 = [r|<a href="re">red</a>|]

-- main2 = parse parser2 "" t2

-- t3 = [r| href="red" r="hello"/>|]
 
-- -- parser3 = many (attrParser)
-- -- parser3 = attrsParser [] >> (string ">" <|> string "/>")

-- main3 = parse parser3 "" t4

-- t4 = [r|<a>a</a>|]
  

-- parser3 :: Stream s m Char => ParsecT s u m (Elem' String)
-- parser3 = elemParser (Just ["a"]) Nothing []

-- could easily rewrite to noneOf ['

-- p = parseOpeningTag Nothing [] >> char '>' >> (manyTill ((IText . (:[])) <$> anyChar) (endTag "a")) >> return () 

-- p = parseOpeningTag Nothing [] >> char '>' >> anyChar >> endTag "a"

-- m = parse parser3 "" t4




-- parser5 :: Stream s m Char => ParsecT s u m (Maybe [Char])
-- parser5 = findNaive (char 'a')

-- -- n = parse (findNaive (string "no fucking way")) "" "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeno fucking waytefffffffffffffffffffff"






-- f :: Stream s m Char => ParsecT s u m (Elem' String)
-- f = elemParser Nothing Nothing []

someFunc :: IO ()
someFunc = do 
  mgrHttps <- newManager tlsManagerSettings
  requ <- parseRequest "https://www.amazon.ca/s?rh=n%3A6205124011&fs=true&ref=lp_6205124011_sar"
  response <- httpLbs requ mgrHttps
  let
    dadBod = (unpack . toStrict . decodeUtf8 . responseBody) response
    parser :: Stream s m Char => ParsecT s u m (Elem' String)
    parser = elemParser (Just ["a"]) Nothing [("class", Just "a-link-normal a-text-normal")]
    parser' = findNaive parser
    result :: Either ParseError (Maybe [Elem' String])
    result = parse parser' "" dadBod 

    r :: [Elem' String]
    r =  (\(Right (Just aList)) -> aList) result

    x :: [String]
    x = catMaybes $ fmap (Map.lookup "href" . attrs) r
    
  -- print (isInfixOf "Amazon" dadBod)
  mapM_ print x

  --to get all
  -- mkRequest (baseUrl <> href) --> html
  
  -- mapM_ print response 



someFunc' :: String -> IO String
someFunc' url = do
  mgrHttps <- newManager tlsManagerSettings
  requ <- parseRequest url
  response <- httpLbs requ mgrHttps
  let
    dadBod = (unpack . toStrict . decodeUtf8 . responseBody) response

  return dadBod
  
baseUrl = "https://www.amazon.ca/"

title :: Stream s m Char => ParsecT s u m String
title = do
  let
    p :: Stream s m Char => ParsecT s u m (Elem' String)
    p = elemParser (Just ["span"]) Nothing ([("id", Just "productTitle"),("class", Just "a-size-large product-title-word-break")])
  e <- p
  return $ innerText' e




el :: String -> [(String, String)] -> ParsecT String () Identity (Elem' String)
el element attrs = elemParser (Just (element:[])) Nothing ((fmap . fmap) Just attrs)

f = el "span" [("id", "priceblock_ourprice"), ("class", "a-size-medium a-color-price priceBlockBuyingPriceString")]

price = do
  x <- f
  return $ innerText' x

price2 = do
  x <- price2p
  return $ innerText' x

price2p = el "span" [("id", "price_inside_buybox"), ("class", "a-size-medium a-color-price")]
  



n = do
  text <- someFunc' "https://www.amazon.ca/Natural-Massager-Anti-aging-Slimming-Firming/dp/B07X7ZFW78/ref=sr_1_5?dchild=1&qid=1618856760&s=beauty&sr=1-5"
  return $ parse (findNaive price) "" text

price2I = do 
  text <- someFunc' "https://www.amazon.ca/Natural-Massager-Anti-aging-Slimming-Firming/dp/B07X7ZFW78/ref=sr_1_5?dchild=1&qid=1618856760&s=beauty&sr=1-5"
  return $ parse (findNaive price2) "" text



---
-- f = el "span" [("id", "priceblock_ourprice"), ("class", "a-size-medium a-color-price priceBlockBuyingPriceString")]
-- price = do
--   x <- f
--   return $ innerText' x
  
-- m = do
--   text <- someFunc' "https://www.amazon.ca/Natural-Massager-Anti-aging-Slimming-Firming/dp/B07X7ZFW78/ref=sr_1_5?dchild=1&qid=1618856760&s=beauty&sr=1-5"
--   return $ (parse (findNaive title) "" text)
--   -- return $ take 200 text
---


brand = el "a" [("id", "bylineInfo")]
  
-- m = do
--   text <- someFunc' "https://www.amazon.ca/Natural-Massager-Anti-aging-Slimming-Firming/dp/B07X7ZFW78/ref=sr_1_5?dchild=1&qid=1618856760&s=beauty&sr=1-5"
--   return $ (parse (findNaive (fmap innerText' brand) "" text))
--   -- return $ take 200 text

parser' :: ParsecT String () Identity (Elem' String) -> IO (Either ParseError (Maybe [String]))
parser' parser =
            do
              text <- someFunc' "https://www.amazon.ca/Natural-Massager-Anti-aging-Slimming-Firming/dp/B07X7ZFW78/ref=sr_1_5?dchild=1&qid=1618856760&s=beauty&sr=1-5"
              return $ parse (findNaive (fmap innerText' parser)) "" text         

module Scrape where

-- Basically just html patterns from testing / courtney market stuff
import Elem.Types (Elem', innerText')
import Elem.ElemHeadParse (hrefParser)
import Find (findNaive)
import Links (maybeUsefulUrl)

import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT, parse)


runScraperOnHtml :: ParsecT String () Identity a -> String -> Maybe [a]
runScraperOnHtml p html = fromRight Nothing $ parse (findNaive $ p) "" html 

runScraperOnHtml1 :: ParsecT String () Identity a -> String -> Maybe a
runScraperOnHtml1 p = (fmap head) . runScraperOnHtml p


{-# DEPRECATED simpleScrape' "from fba project - gives confusing String output" #-}
simpleScrape' :: ParsecT String () Identity String -> String -> String 
simpleScrape' p html = case parse (findNaive p) "" html of
  Right (Just (x:_)) -> x
  Right (Just []) -> "NothingA"
  Right (Nothing) -> "NothingB"
  Left err -> "Nothing" <> show err



clean :: String -> String
clean = undefined -- drop if == ( \n | \" | '\\' )


-- same site is guranteed
allLinks :: String -> ParsecT String () Identity [String] 
allLinks baseUrl = do
  x <- findNaive hrefParser 
  return $ case x of
    Just (x':xs') -> catMaybes $ fmap (maybeUsefulUrl baseUrl) (x':xs')
    Just [] -> []
    Nothing -> [] 

type Name = String -- placeholder
tableItem :: Name -> Elem' String
tableItem = undefined




-- scrapeInnerText :: ParsecT String () Identity (Elem' String) -> String 
-- scrapeInnerText p = case parse (findNaive p) "" body of
--   Right (Just (x:_)) -> innerText' x
--   Right (Just []) -> "Nothing"
--   Right (Nothing) -> "Nothing"
--   Left err -> show err




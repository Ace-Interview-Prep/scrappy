module Scrape where

-- Basically just html patterns from testing / courtney market stuff
import Elem.Types (Elem', innerText')
import Elem.ElemHeadParse (hrefParser)
import Find (findNaive)
import Links (maybeUsefulUrl)

import Data.Maybe (catMaybes)
import Data.Functor.Identity (Identity)
import Text.Parsec (ParsecT, parse)




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




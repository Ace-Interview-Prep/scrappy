{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Scrappy.Scrape where

-- Basically just html patterns from testing / courtney market stuff
import Scrappy.Elem.Types (Elem', innerText')
import Scrappy.Elem.ElemHeadParse (hrefParser, parseOpeningTag)
import Scrappy.Elem.SimpleElemParser (el)
import Scrappy.Elem.ChainHTML ((</>>))
import Scrappy.Find (findNaive)
import Scrappy.Links (Html, maybeUsefulUrl)

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Functor.Identity (Identity)
import Witherable (Filterable, mapMaybe)
import Data.Either (fromRight)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Parsec (Stream, ParsecT, parse, parserZero, anyChar, manyTill, char)


type ScraperT a = ParsecT Html () Identity a 


-- | Upgrade an error to discard parser 
instance Filterable (ParsecT s u f) where
  mapMaybe f ma = do
    x <- ma
    case f x of
      Just a -> return a 
      Nothing -> parserZero


-- | Super common case analysis
coerceMaybeParser :: Maybe a -> ScraperT a
coerceMaybeParser = \case
  Just a -> return a
  Nothing -> parserZero


hoistMaybe :: Applicative m => Maybe a -> MaybeT m a 
hoistMaybe = MaybeT . pure


exists :: ScraperT a -> Html -> Bool 
exists p html = maybe False (const True) $ runScraperOnHtml p html 

scrape :: ScraperT a -> Html -> Maybe [a]
scrape = runScraperOnHtml

scrapeFirst' :: ScraperT a -> Html -> Maybe a
scrapeFirst' f h = case scrape f h of
                    Just (x:xs) -> return x
                    _ -> Nothing 


-- fmap head
getFirstSafe :: Maybe [a] -> Maybe a
getFirstSafe (Just (x:_)) = Just x
getFirstSafe _ = Nothing


getFirstFitSafe :: (a -> Bool) -> Maybe [a] -> Maybe a
getFirstFitSafe f (Just (x:xs)) = findFit f (x:xs) 
getFirstFitSafe _ _ = Nothing 

findFit :: (a -> Bool) -> [a] -> Maybe a
findFit _ [] = Nothing
findFit cond (x:xs) = if cond x then Just x else findFit cond xs 



-- | Find all occurences of a given parsing/scraping pattern
-- | e.g. getHtml' "https://google.ca" >>= return . runScraperOnHtml (el "a" []) , would give all 'a' tag html elements on google.ca  
runScraperOnHtml :: ParsecT String () Identity a -> String -> Maybe [a]
runScraperOnHtml p html = fromRight Nothing $ parse (findNaive $ p) "" html 


runScraperInBody :: ParsecT String () Identity a -> String -> Maybe [a]
runScraperInBody prsr html = fromRight Nothing $ parse (skipToInBody >> findNaive prsr) "" html

skipToInBody :: Stream s m Char => ParsecT s u m ()
skipToInBody = manyTill anyChar (parseOpeningTag (Just ["html"]) [] >> char '>')
               </>> el "head" []
               </>> parseOpeningTag (Just ["body"]) []
               >> char '>'
               >> return () 

  
runScraperOnBody :: ParsecT String () Identity a -> String -> Maybe [a] 
runScraperOnBody prsr html = fromRight Nothing $ parse (skipToBody >> findNaive prsr) "" html 

skipToBody :: Stream s m Char => ParsecT s u m ()
skipToBody = manyTill anyChar (parseOpeningTag (Just ["html"]) [] >> char '>') </>> el "head" [] >> return () 


runScraperOnHtml1 :: ParsecT String () Identity a -> String -> Maybe a
runScraperOnHtml1 p = (fmap head) . runScraperOnHtml p





-- {-# DEPRECATED simpleScrape' "from fba project - gives confusing String output" #-}
-- simpleScrape' :: ParsecT String () Identity String -> String -> String 
-- simpleScrape' p html = case parse (findNaive p) "" html of
--   Right (Just (x:_)) -> x
--   Right (Just []) -> "NothingA"
--   Right (Nothing) -> "NothingB"
--   Left err -> "Nothing" <> show err



-- clean :: String -> String
-- clean = undefined -- drop if == ( \n | \" | '\\' )


-- | uses maybeUsefulUrl to get all links on page pointing only to same site links
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




scrapeFirst :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Maybe a)
scrapeFirst p = do
  x <- findNaive p
  case x of
    Just (x:_) -> return $ Just x
    Nothing -> return $ Nothing

 
findCount :: Stream s m Char => ParsecT s u m a -> ParsecT s u m Int
findCount p = do
  x <- findNaive p
  return $ length (fromMaybe [] x)


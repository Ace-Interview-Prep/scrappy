{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Scrappy.Elem.ChainHTML where



import Scrappy.Find (findNaive)
import Scrappy.Links (maybeUsefulUrl)
import Scrappy.Elem.SimpleElemParser (elemParser)
import Scrappy.Elem.ElemHeadParse (parseOpeningTag, hrefParser)
import Scrappy.Elem.Types (Elem', ShowHTML, ElemHead, Elem, innerText'
                  , matches')

import Control.Monad.Trans.Maybe (MaybeT)
import Text.Parsec (ParsecT, Stream, char, (<|>), many, parserFail, parse, parserZero, string, optional)
import Control.Applicative (some, liftA2)
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)
-- functions for chaining free-range html patterns based on the previous
-- patterns to allow for maximum flexibility 

nl :: Stream s m Char => ParsecT s u m ()
nl = optional (many $ (char '\n' <|> char ' '))

manyHtml p = many $ p <* nl

someHtml p = some $ p <* nl

manyTillHtml_ p end = manyTill_ (p <* nl) end 


htmlTag :: Stream s m Char => ParsecT s u m ElemHead
htmlTag = parseOpeningTag (Just ["html"]) [] 




manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go



clean :: String -> String
clean = undefined -- drop if == ( \n | \" | '\\' )


-- -- same site is guranteed
-- allLinks :: String -> ParsecT String () Identity [String] 
-- allLinks baseUrl = do
--   x <- findNaive hrefParser 
--   return $ case x of
--     Just (x':xs') -> catMaybes $ fmap (maybeUsefulUrl baseUrl) (x':xs')
--     Just [] -> []
--     Nothing -> [] 



mustContain :: ParsecT s u m (Elem' a) -> Int -> ParsecT s u m b -> ParsecT s u m (Elem' a)
mustContain e count pat = do
  out <- e
  case parse (findNaive $ string "Search") "" (innerText' out) of
    Right (Just xs) -> if count > (length xs) then parserZero else return out
    _ -> parserZero
    
-- | An elem head configures the bracketing so this is all we need for
-- | crafting

type Shell =  (Elem, [(String, Maybe String)]) 

-- incomplete
contains'' :: (Stream s m Char, ShowHTML a) => Shell 
           -> ParsecT s u m a
           -> ParsecT s u m [a]
contains'' (e,as) p = matches' <$> elemParser (Just [e]) (Just p) as

parseInShell = contains 

-- | This will be fully removed in the future 
{-# DEPRECATED contains "this should have been called parseInShell from the start, you probably want contains' or containsFirst" #-}
contains :: ParsecT s u m (Elem' a) -> ParsecT String () Identity b -> ParsecT s u m b
contains shell b = do
  x <- shell

  let
    ridNL p = (many (char ' ' <|> char '\n')) >> p 
  
  -- need to skip 
  case parse (ridNL b) "" (innerText' x) of
    Right match -> return match
    Left err -> parserFail (show err)

-- finds multiple matches anywhere inside the passed elem
contains' :: ShowHTML a =>
             ParsecT s u m (Elem' a) 
          -> ParsecT String () Identity b
          -> ParsecT s u m [b]
contains' shell b = do
  x <- shell
  case parse (findNaive b) "" (innerText' x) of
    Right (Just matches) -> pure matches
    Left err -> parserFail (show err)
    Right Nothing -> parserFail "no matches in this container" 

containsFirst :: ShowHTML a =>
                 ParsecT s u m (Elem' a) 
              -> ParsecT String () Identity b
              -> ParsecT s u m b
containsFirst shell b = head <$> contains' shell b


sequenceHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
sequenceHtml p1 p2 = do
  x <- p1
  _ <- many (char ' ' <|> char '\n')
  y <- p2
  return (x, y)

sequenceHtml_ :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b
sequenceHtml_ p1 p2 = do
  _ <- p1
  _ <- many (char ' ' <|> char '\n')
  p2

(</>>) :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b 
(</>>) = sequenceHtml_

(</>>=) :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
(</>>=) = sequenceHtml

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- manyHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
-- manyHtml prsrHtml = (many (char ' ' <|> char '\n')) >> many (fmap snd $ manyTill_ (char ' ' <|> char '\n') prsrHtml)

-- someHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
-- someHtml prsrHtml = many (char ' ' <|> char '\n') >> some prsrHtml




{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-} 

module Scrappy.Template where

import Scrappy.Find

import Text.Parsec
import Control.Applicative (liftA2)
import qualified Data.Map as Map
import qualified Data.Text as T


-- A particular use of this library-module is like a cheap FFI
  -- it is also for this reason that the errors are meant to not be recoverable - an error in a
  -- non-predetermined language is just too wide a case to consider (imo) 

-- This is a module for templating files using Scrappy, like how Flask uses templates
-- TODO: make own package


-- TODO: move manyTill_ , between', between1 into Scrappy.ParsecExtra

manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go


between' :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m [a]
between' open close inside = do
  open
  (x, _) <- manyTill_ inside close
  pure x


between1 :: Stream s m Char => ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m a 
between1 open end match = open *> match <* end
    

templateSyntaxExample :: String 
templateSyntaxExample = "{{>>=name}}"

type Name = String 
templateParser :: Stream s m Char => ParsecT s u m Name
templateParser = 
  between1 (string "{{>>=") (string "}}") validName
  where
    validName :: Stream s m Char => ParsecT s u m Name 
    validName = do
      first <- letter <|> (char '_') <|> (char '-') 
      rest <- many $ alphaNum <|> (char '_') <|> (char '-')
      pure $ first : rest


-- TODO: use template haskell and staticWhich to make this compile time so that it fails
-- if an undefined name does not exist in the Map and can also give warnings when some are not used 
-- Take a String (likely from a file) and replace templates where they exist 
unTemplate :: Map.Map String String -> String -> String
unTemplate tmplMap input = streamEdit templateParser mapLookup input
  where
    mapLookup :: Name -> String
    mapLookup n = case Map.lookup n tmplMap of
      Just v -> v
      Nothing -> error $ "name {{>>=" <> n <> "}}" <> " not defined" 

-- TOD: make this catchable as an IOError 
unTemplateFile :: Map.Map String String -> FilePath -> IO String
unTemplateFile tmplMap fp = do
  s <- readFile fp
  pure $ unTemplate tmplMap s

{-# LANGUAGE FlexibleContexts #-}


module Elem.ElemHeadParse where

import Elem.Types (Elem, Elem', Attrs, AttrsError(IncorrectAttrs)) -- Attr)

import Text.Megaparsec as MParsec (some, manyTill)
import Text.Parsec (Stream, ParsecT, (<|>), string, try, noneOf, parserZero, char, option, space,
                   alphaNum, many1, between, many, letter)
import Data.Map as Map (Map, fromList, lookup) 


-- | needs to use many for multiple links


-- | Safe because it forces parse of the entire ElemHead then pulls if there
-- | Designed for use in findSomeHtml
parseAttrSafe :: Stream s m Char => String -> ParsecT s u m String 
parseAttrSafe attrName = do
  tag <- parseOpeningTag Nothing [(attrName, Nothing)] -- i could in theory pass an expression as value
  case (Map.lookup attrName . snd) tag of
    Nothing -> parserZero
    Just a -> return a

-- | Done like  this so that it reliably is true link and not false positive 
hrefParser :: Stream s m Char => ParsecT s u m String --Link
hrefParser = do
  tag <- parseOpeningTag Nothing [("href", Nothing)] -- i could in theory pass an expression as value
  case (Map.lookup "href" . snd) tag of
    Nothing -> parserZero
    Just a -> return a
-- snd OR fmap snd for multiple then analyze URI

attrParser' :: Stream s m Char => String -> (String -> Bool) -> ParsecT s u m String --Link
attrParser' attrib predicate = do
  tag <- parseOpeningTag Nothing [(attrib, Nothing)] -- i could in theory pass an expression as value
  case (Map.lookup "href" . snd) tag of
    Nothing -> parserZero
    Just a -> if predicate a then return a else parserZero 

-- | Allows parsing with high level predicate 
hrefParser' :: Stream s m Char => (String -> Bool) -> ParsecT s u m String --Link
hrefParser' predicate = do
  tag <- parseOpeningTag Nothing [("href", Nothing)] -- i could in theory pass an expression as value
  case (Map.lookup "href" . snd) tag of
    Nothing -> parserZero
    Just a -> if predicate a then return a else parserZero 
-- snd OR fmap snd for multiple then analyze URI

-- Does between have an answer to my problem of non-infinite parsers?
-- between' (parseOpeningTag) (endTag) innerParser ... or something


--also could be with '' not just ""
-- | In future should add replace of apostrophe and similar issues to corresponding html representations
attrValue :: Stream s m Char => ParsecT s u m [Char]
attrValue = between (char '"' <|> char '\'') (char '"' <|> char '\'') (many1 (noneOf ['"', '\'']))

-- -- this could be extensible to scrapePDFLink
-- attrValue' :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
-- attrValue' parser = between (char '"') (char '"') parser

-- | Both attrValue functions mimic map functionality 

attrValuesExist :: [(String, String)] -> [(String, Maybe String)] -> Bool 
attrValuesExist _ [] = True-- Either AttrsError [(String, String)]
attrValuesExist attrsOut (nextAttr:attrsIn)
  | attrValueExists attrsOut nextAttr = True && (attrValuesExist attrsOut attrsIn)
  | otherwise = False 

attrValueExists :: [(String, String)] -> (String, Maybe String) -> Bool
attrValueExists [] _ = False 
attrValueExists (attrF:attrsOut) nextAttr-- (AttrPair nextAttrP:attrsIn)
  | fst attrF == fst nextAttr && snd nextAttr == Nothing = True
  | fst attrF == fst nextAttr && snd nextAttr == (Just (snd attrF)) = True
  | otherwise = attrValueExists attrsOut nextAttr


-- -- | Doesn't check for value of attr
-- attrNamesExist :: [(String, String)] ->  [AttrsP] -> Either AttrsError [(String, String)]
-- -- attrNamesExist attrsOut [AnyAttr] = Right attrsOut --relevant??
-- attrNamesExist attrsOut [] = Right attrsOut 
-- attrNamesExist attrsOut (Attr nextAttr:attrsIn)
  
--    elem nextAttr (fmap fst attrsOut) = attrNamesExist attrsOut attrsIn
--    otherwise = Left IncorrectAttrs 
--   -- Im not sure this will continue to the next element though

-- -- | With attrs, we must parse to a map regardless of actual value due to lack of ordering then
-- -- |  should do lookup


attrName :: Stream s m Char => ParsecT s u m String 
attrName = some (alphaNum <|> char '-')
-- | Need lower|UPPER case insensitivity
           --

-- SPACE, ("), ('), (>), (/), (=)

-- | for generalization sake
attrParser :: Stream s m Char => ParsecT s u m (String, String)
attrParser = do
      _ <- space

      --re-implement anyAttr
        --needs to include weird edge cases
      attrName' <- attrName
      content <- option "" (char '=' >> attrValue)
      return (attrName', content)
                                  -- [AttrsPNew]
attrsParser :: Stream s m Char =>
               [(String, Maybe String)] -- Maybe (ParsecT s u m String)
            -> ParsecT s u m (Either AttrsError (Map String String))
            --change to Either AttrsError (Map String String) 
attrsParser attrs = do
  -- attrPairs <- MParsec.manyTill attrParser {- this needs to also handle -} (char '/' <|> char '>')
  attrPairs <- many attrParser -- (char '>' <|> char '/')
  let
    attrPairsMap = fromList attrPairs
  case isAttrsMatch attrPairsMap attrs of
    True -> return $ Right attrPairsMap
    False -> return $ Left IncorrectAttrs


isAttrsMatch' :: Map String String -> [(String, Maybe String)] -> Bool
isAttrsMatch' _ [] = True 
isAttrsMatch' mapAttr ((name, maybeVal):desired) 
  | Map.lookup name mapAttr == Nothing = False 
  | (Map.lookup name mapAttr == maybeVal) || (maybeVal == Nothing) = isAttrsMatch mapAttr desired 

isAttrsMatch :: Map String String -> [(String, Maybe String)] -> Bool
isAttrsMatch _ [] = True
isAttrsMatch mapAttr ((name, maybeVal): desired) = case maybeVal of
  Just val ->
    case Map.lookup name mapAttr of
      Just valFromKey -> if val /= valFromKey then False else isAttrsMatch mapAttr desired
      Nothing -> False
        
  Nothing ->
    case Map.lookup name mapAttr of
      Just irrValFromKey {- we only care about the name -} -> isAttrsMatch mapAttr desired
      Nothing -> False
  
-- | NOTES
-- if href="#" on form -> just means scroll to top

-- | May rename parseOpeningTag to elemHeadParser
  -- |  -> Case of input tag: <input ...."> DONE ie no innerhtml or end tag
  -- |     then this would be more efficient or even maybe we should add an option via
  -- |     a  datatype: InnerTextOpts a = DoesntExist --efficient parser | AnyText | ParserText a
parseOpeningTag :: Stream s m Char => Maybe [Elem] -> [(String, Maybe String)] -> ParsecT s u m (Elem, Attrs)
parseOpeningTag elemOpts attrsSubset = do
  -- _ <- MParsec.manyTill anyToken (char '<' >> elemOpts >> attrsParser attrsSubset) -- the buildElemsOpts [Elem]
  _ <- char '<'
  elem <- mkElemtagParser elemOpts
  attrs <- attrsParser attrsSubset

  case attrs of
    Left IncorrectAttrs -> parserZero
    Right whateva -> return (elem, whateva)

-- | For elemsOpts, will either be
-- | Parser: (anyChar)
-- | Parser: (buildElemsOpts elems)

-- parseOpeningTagTutorial :: ParsecT s u m (String, [(String, String)])
-- parseOpeningTagTutorial = do
--   skipChar '<'
--   elem <- some letter
--   attrs <- manyTill attrParser 

-- attrParser :: Stream s m Char => ParsecT s u m (String, String)
-- attrParser = do
--   _ <- space
--   attrName' <- attrName
--   content <- option "" (char '=' >> attrValue)
--   return (attrName', content)


mkElemtagParser :: Stream s m Char => Maybe [Elem] -> ParsecT s u m String
mkElemtagParser x = case x of
                   -- Nothing -> MParsec.some (noneOf [' ', '>'])
                      --commented out in case below is wrong
                      Nothing -> MParsec.some letter
                      Just elemsOpts -> buildElemsOpts elemsOpts


-- | FUTURE USE CASES: buildElemsOpts :: [ParsecT s u m a] -> ParsecT s u m a -- using <|>
buildElemsOpts :: Stream s m Char => [Elem] -> ParsecT s u m String
-- buildElemsOpts [] = <----- i dont think i need this
buildElemsOpts [] = parserZero
buildElemsOpts (x:elemsAllow) = try (string x) <|> (buildElemsOpts elemsAllow)


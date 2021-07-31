{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Elem.TreeElemParser where

import Control.Monad.IO.Class

import Elem.ElemHeadParse (parseOpeningTag, hrefParser')
import Elem.Types (Elem, Attrs, ElemHead, TreeHTML(TreeHTML), HTMLMatcher (IText, Element, Match)
                  , InnerTextHTMLTree(InnerTextHTMLTree), innerTree, innerText, matches, GroupHtml
                  , Elem', TreeIndex, attrs, elTag, ShowHTML, showH, _innerTree', matches'
                  , ElementRep, mkGH, innerText', _innerText, _matches, foldFuncTrup
                  , UrlPagination(..), enoughMatchesTree, selfClosingTextful, endTag)


import Elem.SimpleElemParser (elemParser)
import Find (findNaive)

import Control.Monad (when)
import Text.Megaparsec as MParsec (many, manyTill_, skipManyTill, manyTill, some)
import Text.Parsec (Stream, ParsecT, anyChar, (<|>), try, parserZero, parserFail, string, parse, char)
import qualified Data.Map as Map (Map, toList) 
import Data.Graph (Tree (Node), Forest)
import Text.URI as URI
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe, fromJust)
import Data.List
import Data.Text (Text, splitOn)


data Many a = Many a | One a 

treeLookupIdx :: TreeIndex -> Forest a -> a
treeLookupIdx = undefined

--------------------------------------------------------------------------------------------------------------------
---Generalizations----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

htmlGenParser :: (Stream s m Char, ShowHTML a)
              => ParsecT s u m a
              -> ParsecT s u m (TreeHTML a)
              -> ParsecT s u m (HTMLMatcher TreeHTML a)
htmlGenParser a parseTreeH = (Match <$> try a)
                             <|> (Element <$> try parseTreeH)
                             <|> (fmap (IText . (:[])) anyChar)

-- | Library function for when you want an exact match, if 3 of ElemHead A then it looks for 3 Elemhead A
 -- accumMaybe' :: [HTMLMatcher] -> ParsecT s u m a
specificForest :: (Stream s m Char, ShowHTML a) =>
                  [Tree ElemHead]
               -> ParsecT s u m a
               -> ParsecT s u m [HTMLMatcher TreeHTML a]
specificForest [] _ = return [] --or could be allowing for tail text
specificForest (x:xs) match = do
  y <- htmlGenParser match (nodeToTreeElemExpr x match)
  ys <- case y of
     Element _ -> specificForest xs match
     _ -> specificForest (x:xs) match
  return (y : ys) 


  
nodeToTreeElemExpr :: (Stream s m Char, ShowHTML a) =>
                      Tree ElemHead
                   -> ParsecT s u m a
                   -> ParsecT s u m (TreeHTML a)
nodeToTreeElemExpr (Node (elem, attrs) subTree) match =
  treeElemParserSpecific (Just match) elem (Map.toList attrs) subTree



-----------------------------------------------------------------------------------------------------------------------------Main---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

treeElemParser :: (Stream s m Char, ShowHTML a) =>
                   Maybe [Elem]
                -> Maybe (ParsecT s u m a)
                -> [(String, Maybe String)]
                -> ParsecT s u m (TreeHTML a)
treeElemParser elemOpts matchh attrsSubset = do
  e <- treeElemParser' elemOpts matchh attrsSubset
  when (length (matches' e) < (case matchh of { Nothing -> 0; _ -> 1 })) (parserFail "not enough matches")
  return e


treeElemParser' :: (Stream s m Char, ShowHTML a) =>
                  Maybe [Elem]
               -> Maybe (ParsecT s u m a)
               -> [(String, Maybe String)]
               -> ParsecT s u m (TreeHTML a)
treeElemParser'  elemOpts matchh attrsSubset = do
 (elem', attrs') <- parseOpeningTag elemOpts attrsSubset
 -- let
   -- f = f :: ShowHTML a => [HTMLMatcher TreeHTML a]
   -- f = char '>' >> manyTill (htmlGenParserFlex matchh) (endTag elem')
 (inText, matchBook, treees) <- fmap (foldr foldFuncTrup mempty)
                                $ (try (string "/>") >> return [])  
                                <|> (try $ innerElemParser2 elem' matchh)
                                <|> (selfClosingTextful matchh)
 return $ TreeHTML elem' attrs' matchBook (reverse inText) treees

innerElemParser2 :: (ShowHTML a, Stream s m Char) =>
                   String
                -> Maybe (ParsecT s u m a)
                -> ParsecT s u m [HTMLMatcher TreeHTML a]
innerElemParser2 eTag innerSpec = char '>'
                                  -- >> manyTill (try (Element <$> treeElemParser Nothing innerSpec [])) (try (endTag eTag))
                                  >> manyTill (try (Match <$> (fromMaybe parserZero innerSpec))
                                               <|> try (Element <$> treeElemParser Nothing innerSpec [])
                                               <|> ((IText . (:[])) <$> anyChar)) (try $ endTag eTag)

--- NEXT 3 ARE NOT IN USE, useful for API?

treeElemParserAnyInside :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a) -> ParsecT s u m (TreeHTML a)
treeElemParserAnyInside match = treeElemParser Nothing match []

-- this implementation would cause issues for when we want to check equality of trees
-- we would need to set the inside tree element parser + we would also need to think about how to     handle matches -->> maybe check after for matches > 0?
htmlGenParserFlex :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a) -> ParsecT s u m (HTMLMatcher TreeHTML a)
htmlGenParserFlex a = (try (Match <$> (fromMaybe parserZero a)))
                      <|> try (Element <$> treeElemParser Nothing a [])   --(treeElemParserAnyInside a))
                      <|> ((IText . (:[])) <$> anyChar)

--------------------------------------------------------------------------------------------------------------------
-------------------------------------------Groupings--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | Returns a minimum of 2 --> almost like `same` should be function ; same :: a -> [a] to be applied to some doc/String
-- | note: not sure if this exists but here's where we could handle iterating names of attributes 
-- | Can generalize to ElementRep e
findHtmlGroup  :: (Stream s m Char, ShowHTML a)
               => Maybe [Elem]
               ->  Maybe (ParsecT s u m a)
               -> [(String, Maybe String)]
               -> ParsecT s u m (GroupHtml TreeHTML a)
findHtmlGroup elemOpts matchh attrsSubset = do
  treeH <- treeElemParser elemOpts matchh attrsSubset
  treeHs <- some (try $ Element <$> findSameTreeH matchh treeH <|> (IText <$> string "\n"))

  let
    elems [] = []
    elems (x:xs) = case x of
      Element e -> e : (elems xs)
      IText chr -> elems xs

  return $ mkGH (treeH : (elems treeHs)) 

  -- (_, treeH) <- manyTill_ (anyChar) (try $ treeElemParser elemOpts matchh attrsSubset)
  -- treeHs <- some (try $ findSameTreeH matchh treeH)
  -- return $ mkGH (treeH : treeHs) 


anyHtmlGroup :: (ShowHTML a, Stream s m Char) => ParsecT s u m (GroupHtml TreeHTML a)
anyHtmlGroup = findHtmlGroup Nothing Nothing [] 


-- Maybe this func should go in Find.hs
-- 0 -> Nothing
findAllSpaceMutExGroups :: (ShowHTML a, Stream s m Char) => ParsecT s u m (Maybe [GroupHtml TreeHTML a])
findAllSpaceMutExGroups = findNaive anyHtmlGroup 




findAllMutExGroups' = undefined -- prime in name until renaming errors complete
-- deals with cases where attr:selected="true" exists since there will be two subgroups that
-- require concatenations

-- find == runParserOnHtml :: ParsecT s u m (Maybe [a]) ; a ~ GroupHtml b

-- Note: If we can find all groups, then we can find all non-groups ~ tree/display functionality 


-- findSomeHtmlNaive (try findAnyHtmlGroup) htmlText 


------------------------------------------------------------------------------------------------------------------------------------------------------Numerically Flexible Pattern Matching On Specific Elements----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | Interface to find same element    
findSameTreeH :: (Stream s m Char, ShowHTML a)
              => Maybe (ParsecT s u m a)
              -> TreeHTML a
              -> ParsecT s u m (TreeHTML a)
findSameTreeH matchh treeH = treeElemParserSpecific matchh (elTag treeH) (Map.toList $ attrs treeH) (_innerTree' treeH)

-- I could do 2 things at the same time as calling findSameTreeH : use in findNaive, use in `some` 

type SubTree a = [Tree a]
treeElemParserSpecific :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a)
                       -> Elem
                       -> [(String, String)]
                       -> SubTree ElemHead
                       -> ParsecT s u m (TreeHTML a)
treeElemParserSpecific matchh elem attrs' subTree = do
  (tag, attrsOut) <- parseOpeningTag (Just [elem]) ((fmap . fmap) Just attrs')
  x <- char '>' >> specificRepetitiveForest (groupify subTree []) (fromMaybe parserZero matchh)
  -- can be followed by whatever
  (y, _) <- manyTill_ (htmlGenParserFlex matchh) (endTag tag)
  let
    -- | need to ensure all the trees are in order 
    (inText, matchBook, treees) = foldr foldFuncTrup mempty (x <> y)
  return $ TreeHTML tag attrsOut matchBook (reverse inText) treees--(_matches itr) (_innerText itr) (innerTree itr)


specificRepetitiveForest :: (Stream s m Char, ShowHTML a)
                         => [Many (Tree ElemHead)]
                         -> ParsecT s u m a
                         -> ParsecT s u m [HTMLMatcher TreeHTML a]
specificRepetitiveForest [] _ = return []
specificRepetitiveForest (manyElHead:manyElHeads) match = do
  y <- htmlGenParserRepeat match (multiTreeElemHeadParser match manyElHead)
  ys <- case y of
    -- Discard last parsed pattern and go to next element formula on success of `y` 
    ((Element _):xs') -> specificRepetitiveForest manyElHeads match
    _                -> specificRepetitiveForest (manyElHead:manyElHeads) match
  -- return all results 
  return (y <> ys)
  
-- | What if above was of type :: [Many (Tree a)] -> ParsecT s u m [HTMLMatcher a]

-- [Node a _, Node b _, Node c _, Node d _, Node e _]  -> [Many tree1, Many tree2, One tree3]


-- | Is able to repeat / execute any pattern that returns multiple elements of same type
-- |(see manyTreeElemHeadParser)
htmlGenParserRepeat :: (Stream s m Char, ShowHTML a) =>
                       ParsecT s u m a
                    -> ParsecT s u m [TreeHTML a]
                    -> ParsecT s u m [HTMLMatcher TreeHTML a]
htmlGenParserRepeat match parseTreeHs =
  (do
      x <- try match
      return $ (Match x):[]
  )
  <|>
  (
    (fmap . fmap) Element parseTreeHs
  )
  -- just allows for singleton creation (x:[])
  <|>
  (do
      x <- anyChar
      return (IText (x:[]):[])
  )
                                   -- (((IText . (:[])) <$> anyChar) >>= return . flip (:) [])
                                   -- (Match <$> try a) >>= return . flip (:) [])

-- | This is all I actually need , no need for recursion here, since thats already done in top level func
multiTreeElemHeadParser :: (Stream s m Char, ShowHTML a) =>
                          ParsecT s u m a
                       -> Many (Tree ElemHead)
                       -> ParsecT s u m [TreeHTML a]
multiTreeElemHeadParser match mTree = case mTree of
  Many (Node (elem, attrs) subTree) ->
    do
      dirty <- MParsec.many ( (Element <$> (treeElemParserSpecific (Just match) elem (Map.toList attrs) subTree))
                              -- these two characters may validly separate elements
                              <|> (IText <$> string "\n")
                              <|> (IText <$> string " ")
                            )
      let
        isElem (Element _) = True
        isElem _ = False
        cleanse dirt = filter isElem dirt
        strip (Element x) = x
      return $ (fmap strip . cleanse) dirty
    
    -- MParsec.many (treeElemParserSpecific (Just match) elem (Map.toList attrs) subTree)
    -- -- this is i think the one area where we would be expecting "exactly the described elements in order and
    -- -- nothing else"
    --    -- > But there would be '\n' characters 
    
  One (Node (elem, attrs) subTree) ->
    treeElemParserSpecific (Just match) elem (Map.toList attrs) subTree
                                      >>= return . flip (:) [] -- like return . (\x -> x :[])


groupify :: Eq a => [Tree a] -> [Many (Tree a)] -> [Many (Tree a)]
groupify (tree:forest) [] = groupify forest (One tree:[])
groupify [] acc = acc
groupify ((Node elemHead subForest):forest) (mTree:acc) =
  case mTree of
    One (Node elemHead2 subForest2) ->
      if elemHead == elemHead2
      then groupify forest ((Many (Node elemHead2 subForest2)):acc)
      else groupify forest ((One (Node elemHead subForest)) : (One (Node elemHead2 subForest)) : acc)
      --  we want to ensure then that this One constructor isn't touched again, we need to create
      --  another One constructor with the incoming `tree` that was peeled off
    Many (Node elemHead2' subForest2') ->
      if elemHead == elemHead2'
      then groupify forest ((Many (Node elemHead2' subForest2')):acc)
      else groupify forest ((One (Node elemHead subForest))
                            : (Many (Node elemHead2' subForest2'))
                            : acc)
    
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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

import Elem.ChainHTML (someHtml, manyHtml)
import Elem.SimpleElemParser (elemParser)
import Find (findNaive)

import Control.Monad (when)
import Text.Megaparsec as MParsec (many, manyTill_, skipManyTill, manyTill, some)
import Text.Parsec (Stream, ParsecT, anyChar, (<|>), try, parserZero, parserFail, string, parse, char, noneOf)
import qualified Data.Map as Map (Map, toList) 
import Data.Graph (Tree (Node), Forest)
import Text.URI as URI
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe, fromJust)
import Data.List
import Data.Text (Text, splitOn)


data Many a = Many a | One a deriving Show

treeLookupIdx :: TreeIndex -> Forest a -> a
treeLookupIdx = undefined


-------------------------------------------------------------------------------------------------------------------
----------------------Top Level Functions-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





treeElemParser :: (Stream s m Char, ShowHTML a) =>
                   Maybe [Elem]
                -> Maybe (ParsecT s u m a)
                -> [(String, Maybe String)]
                -> ParsecT s u m (TreeHTML a)
treeElemParser elemOpts matchh attrsSubset = do
  e <- treeElemParser' elemOpts matchh attrsSubset
  when (length (matches' e) < (case matchh of { Nothing -> 0; _ -> 1 })) (parserFail "not enough matches")
  return e


selfClosing :: [String]
selfClosing = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]


-- | Used by treeElemParser, is not an interface, use treeElemParser
treeElemParser' :: (Stream s m Char, ShowHTML a) =>
                  Maybe [Elem]
               -> Maybe (ParsecT s u m a)
               -> [(String, Maybe String)]
               -> ParsecT s u m (TreeHTML a)
treeElemParser'  elemOpts matchh attrsSubset = do
 (elem', attrs') <- parseOpeningTag elemOpts attrsSubset
 case elem elem' selfClosing of
   True -> do
      (try (string ">") <|> string "/>")
      case matchh of
        Nothing ->  return $ TreeHTML elem' attrs' mempty mempty mempty
        Just _ -> parserZero 

     -- ((try string ">") <|> string "/>") >> return TreeHTML elem' attrs' mempty mempty mempty
   False -> do 
     (inText, matchBook, treees) <- fmap (foldr foldFuncTrup mempty)
                                    $ (try (string "/>") >> return [])  
                                    <|> (try $ innerElemParser2 elem' matchh)
                                    <|> (selfClosingTextful matchh)
     return $ TreeHTML elem' attrs' matchBook (reverse inText) (reverse treees)

-------------------------------------------------------------------------------------------------------------------

-- | The real difference between (htmlGroup _ _ _) and specificRepetitiveForest is a matter of if we accept the next
-- | piece to be a new discovery to match on or if we are in that process of matching what we just found


-- htmlGroup --calls--> treeElemParser >>= (many treeSpecific --calls--> specificRepForest) 

type SubTree a = [Tree a]
treeElemParserSpecific :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a)
                       -> Elem
                       -> [(String, String)]
                       -> SubTree ElemHead
                       -> ParsecT s u m (TreeHTML a)
treeElemParserSpecific matchh elem' attrs' subTree = do
  (tag, attrsOut) <- parseOpeningTag (Just [elem']) ((fmap . fmap) Just attrs')
  case elem tag selfClosing of
    True -> if not $ null subTree then undefined else do
      (try (string ">") <|> string "/>")
      case matchh of
        Nothing ->  return $ TreeHTML tag attrsOut mempty mempty mempty
        Just _ -> parserZero 
    False -> do 
      char '>'
      txt <- many (noneOf ['<'])
      x <- specificRepetitiveForest (reverse $ groupify subTree []) (fromMaybe parserZero matchh)
      endTag tag
      --can be followed by whatever: 
      -- (y, _) <- manyTill_ (htmlGenParserFlex matchh) (endTag tag) 
      let
        -- | need to ensure all the trees are in order 
        (inText, matchBook, treees) = foldr foldFuncTrup mempty (x)
      return $ TreeHTML tag attrsOut matchBook (reverse inText) (reverse treees)--(_matches itr) (_innerText itr) (innerTree itr)


-- | IS THIS IN THE RIGHT ORDER OR DOES IT NEED TO BE REVERSED?
-- | Creates a simplified set of instructions for parsing a very specific Tree structure 
groupify :: Eq a => [Tree a] -> [Many (Tree a)] -> [Many (Tree a)]
groupify [] acc = acc
groupify (tree:forest) [] = groupify forest (One tree:[])
groupify ((Node elemHead subForest):forest) (mTree:acc) =
  case mTree of
    One (Node elemHeadPrev subForestPrev) ->
      if elemHead == elemHeadPrev
      -- | here is maybe where I could add in checking if forests are equal ? 
      then groupify forest $ ((Many (Node elemHeadPrev subForestPrev)):acc)
      else groupify forest $ ((One (Node elemHead subForest)) : (One (Node elemHeadPrev subForestPrev)) : acc)
      --  we want to ensure then that this One constructor isn't touched again, we need to create
      --  another One constructor with the incoming `tree` that was peeled off
    Many (Node elemHeadPrev subForestPrev) ->
      if elemHead == elemHeadPrev
      then groupify forest ((Many (Node elemHeadPrev subForestPrev)):acc)
      else groupify forest ((One (Node elemHead subForest))
                            : (Many (Node elemHeadPrev subForestPrev))
                            : acc)



-------------------------------------------------------------------------------------------------------------------


-- | Returns a minimum of 2 --> almost like `same` should be function ; same :: a -> [a] to be applied to some doc/String
-- | note: not sure if this exists but here's where we could handle iterating names of attributes 
-- | Can generalize to ElementRep e
htmlGroup  :: (Stream s m Char, ShowHTML a)
               => Maybe [Elem]
               ->  Maybe (ParsecT s u m a)
               -> [(String, Maybe String)]
               -> ParsecT s u m (GroupHtml TreeHTML a)
htmlGroup elemOpts matchh attrsSubset = 
  -- Not sure about the order yet tho
  fmap mkGH $ try (treeElemParser elemOpts matchh attrsSubset
                   >>= (\treeH -> fmap (treeH :) (some (try $ sameTreeH matchh treeH))))
   





--------------------------------------------------------------------------------------------------------------------
---Generalizations----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



specificRepetitiveForest :: (Stream s m Char, ShowHTML a)
                         => [Many (Tree ElemHead)]
                         -> ParsecT s u m a
                         -> ParsecT s u m [HTMLMatcher TreeHTML a]
specificRepetitiveForest [] _ = return []
specificRepetitiveForest (manyElHead:manyElHeads) match = do
  let
    -- funcP :: (ShowHTML a, Stream s m Char) => ParsecT s u m [TreeHTML a]
    funcP = multiTreeElemHeadParser match manyElHead
  y <- htmlGenParserRepeat match funcP -- this literally just allows for matching on multiple elems too
  ys <- case y of
    -- Discard last parsed pattern and go to next element formula on success of `y` 
    ((Element _):xs') -> specificRepetitiveForest manyElHeads match
    _                -> specificRepetitiveForest (manyElHead:manyElHeads) match
  -- return all results 
  return (y <> ys)


-- | Is able to repeat / execute any pattern that returns multiple elements of same type
-- |(see manyTreeElemHeadParser)
htmlGenParserRepeat :: (Stream s m Char, ShowHTML a) =>
                       ParsecT s u m a
                    -> ParsecT s u m [TreeHTML a]
                    -> ParsecT s u m [HTMLMatcher TreeHTML a]
htmlGenParserRepeat match parsesTreeHs =
  (do { x <- try match;  return $ (Match x):[] }) 
  <|> ((fmap . fmap) Element parsesTreeHs) -- list of elements, could be single element or multiple 
  <|> (do { x <- anyChar; return (IText (x:[]):[]) }) -- just allows for singleton creation (x:[])


-- | This is all I actually need , no need for recursion here, since thats already done in top level func
multiTreeElemHeadParser :: (Stream s m Char, ShowHTML a) =>
                          ParsecT s u m a
                       -> Many (Tree ElemHead)
                       -> ParsecT s u m [TreeHTML a]
multiTreeElemHeadParser match mTree = case mTree of
  Many (Node (elem, attrs) subTree) ->
    manyHtml (treeElemParserSpecific (Just match) elem (Map.toList attrs) subTree)
  One (Node (elem, attrs) subTree) ->
    treeElemParserSpecific (Just match) elem (Map.toList attrs) subTree >>= return . flip (:) []
                                                                        -- like return . (\x -> x :[])



-- | Interface to find same element    
sameTreeH :: (Stream s m Char, ShowHTML a)
              => Maybe (ParsecT s u m a)
              -> TreeHTML a
              -> ParsecT s u m (TreeHTML a)
sameTreeH matchh treeH = treeElemParserSpecific matchh (elTag treeH) (Map.toList $ attrs treeH) (_innerTree' treeH)

-- I could do 2 things at the same time as calling findSameTreeH : use in findNaive, use in `some` 

                                      

-- this implementation would cause issues for when we want to check equality of trees
-- we would need to set the inside tree element parser + we would also need to think about how to     handle matches -->> maybe check after for matches > 0?
htmlGenParserFlex :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a) -> ParsecT s u m (HTMLMatcher TreeHTML a)
htmlGenParserFlex a = (try (Match <$> (fromMaybe parserZero a)))
                      <|> try (Element <$> treeElemParser Nothing a [])   --(treeElemParserAnyInside a))
                      <|> ((IText . (:[])) <$> anyChar)
    







--- Not in use: ----------------------------------------------------------------------------------------------------








htmlGenParser :: (Stream s m Char, ShowHTML a)
              => ParsecT s u m a
              -> ParsecT s u m (TreeHTML a)
              -> ParsecT s u m (HTMLMatcher TreeHTML a)
htmlGenParser a parseTreeH = (Match <$> try a)
                             <|> (Element <$> try parseTreeH)
                             <|> (fmap (IText . (:[])) anyChar)




{-# DEPRECATED specificForest "you likely need specificRepetitiveForest" #-}
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


-- | Used by treeElemParser' 
innerElemParser2 :: (ShowHTML a, Stream s m Char) =>
                   String
                -> Maybe (ParsecT s u m a)
                -> ParsecT s u m [HTMLMatcher TreeHTML a]
innerElemParser2 eTag innerSpec = char '>'
                                  -- >> manyTill (try (Element <$> treeElemParser Nothing innerSpec [])) (try (endTag eTag))
                                  >> manyTill (try (Match <$> (fromMaybe parserZero innerSpec))
                                               <|> try (Element <$> treeElemParser' Nothing innerSpec [])
                                               <|> ((IText . (:[])) <$> anyChar)) (try $ endTag eTag)

--- NEXT 3 ARE NOT IN USE, useful for API?

treeElemParserAnyInside :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a) -> ParsecT s u m (TreeHTML a)
treeElemParserAnyInside match = treeElemParser Nothing match []

--------------------------------------------------------------------------------------------------------------------
-------------------------------------------Groupings--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  -- (_, treeH) <- manyTill_ (anyChar) (try $ treeElemParser elemOpts matchh attrsSubset)
  -- treeHs <- some (try $ findSameTreeH matchh treeH)
  -- return $ mkGH (treeH : treeHs) 


anyHtmlGroup :: (ShowHTML a, Stream s m Char) => ParsecT s u m (GroupHtml TreeHTML a)
anyHtmlGroup = htmlGroup Nothing Nothing [] 


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

  
-- | What if above was of type :: [Many (Tree a)] -> ParsecT s u m [HTMLMatcher a]

-- [Node a _, Node b _, Node c _, Node d _, Node e _]  -> [Many tree1, Many tree2, One tree3]

                                   -- (((IText . (:[])) <$> anyChar) >>= return . flip (:) [])
                                   -- (Match <$> try a) >>= return . flip (:) [])

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

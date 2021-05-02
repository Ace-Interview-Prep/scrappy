{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module BuildActions where


import Replace.Megaparsec (findAll)

import Elem.SimpleElemParser (elemParser)
-- import Elem.ElemHeadParse 
import Elem.Types (TreeHTML, Elem, Elem', ElemHead, ElementRep, Attrs, ShowHTML, innerText', elTag, attrs, UrlPagination(..))
import Elem.ElemHeadParse (hrefParser')
-- import Links (Link, Option, Namespace, UrlPagination(..), maybeUsefulUrl)

import Links (maybeUsefulUrl)
import Find (findNaive)

import Text.Parsec (ParsecT, ParseError, Stream, many, parse, string, (<|>), parserZero)
import Text.Parsec.Error (Message (Message))
import Data.Text (Text, pack)
import Data.Map as Map (Map, insert, adjust, findWithDefault, toList, fromList, lookup, union, empty)
import Data.Maybe (fromMaybe, fromJust, catMaybes)
import qualified Text.URI as URI  
import Data.Char (digitToInt)
import Data.List (isPrefixOf)





-- | This process for processing forms to URLs is done by creating 4 different flows

-- | 1. 1 =: 1 ; single valid value for the eventual key ;; typically for verification purposes
-- | 2. 1 =: { Set of Strings } ; we have some amount of Text inputs which allow arbitrary input ;;
--    --> we use this to skew search results to our best efforts through info on domain/keywords
-- | 3 -- A) Select-like Elements
-- |   -- B) Radio Elements

-- | 3A & 3B result in same type eventually -> Map (Namespace/Key) [Option]
    
    --optimal solution begins generating "stream" of urls on new instance of (2) 

-- | In our business logic, it revolves around 2. ; we have maybe no way of knowing for sure which
-- | textinput is which and how they interrelate
-- |    -> (eg. CASE1: 1st is paper title, 2nd is author\
-- |            CASE2: Only 1 text input for any)

-- | We assume 1st is most likely but if it fails or gives unusual responses then we go to next
-- | But for lack of free development time we are hoping this just works the first time and doesn't
-- | need a ton of attention/error handling

--for interacting with the web page solely through HTML







--temp
type SelectElem = Elem'
-- I could use datakinds to promote "select"



-- | Name and Namespace are really same shit; might just converge
-- | Refer to literally "name" attribute
type Namespace = Text

-- | This is an operationally focused type where
-- | a certain namespace is found to have n num of Options
type Option = Text 


-- | More for show / reasoning rn .. non-optimal
data QParams = Opt (Map Namespace [Option]) | SimpleKV (Text, Text)




-- pageKey=param
type PageKey = Text





--PLACEHOLDER
type ReqBody = String


type Form = Text -- Meant to be 

-- | I believe this could instead compose Name/Namespace
-- | OR!! Is the [String] the list of Options? 
type FormOptionRadio = Map String [String] -- ?
type FormOptionRadio' = Map Namespace [Option]


-- Note: Upon further research, it would be useful to generalize to FormOption which reps
-- a well formed input to a URL generator
  -- the only difference for type="radio" is how we get this info

  -- radio:

         -- elem + elem + << .. >> + elem + << .. >> + elem

  -- select

         -- InContext $ (elem + elem + elem + elem)


  -- but both eval to a Query Param with options

  -- We could even have:
                      




scrapeUwoDbLinks :: ParsecT s u m [String]
scrapeUwoDbLinks = undefined


-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
-----Above is outside of research websites domain and below is actions on resDB's------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




inputTypesCondensed = []

data FormRes = FormLinks [Text]

-- type Hrefs = Link
-- data PageLinker = PL' URL [FormRes] [Hrefs]
-- |since it can have multiple forms
-- | This is more for my understanding of the bigger picture of links management
  -- if say:
  --  -> we found ALL forms and created ALL links
  -- but this would be full of trash ofc

-- 2 Options
-- 1) get next page -> do ...
--  1.5) pull from page >>cond=> get x page -> do ...
-- 2) pull from page -> pdf

-- 3) Error handling

-- | A key goal of the link management system is to allow for precedence ; ie we've found browsing
-- | page but prefer search page
   -- therefore: we continue to look for hrefs in such a case until we've exhausted the basic site
   -- tree
type HrefsOfPage = [Text]

data HrefsSite = HrefsSite [HrefsOfPage]

type URL = Text
data PageLinker' = PL URL [FormRes] [HrefsSite]


-- runParserOnHTML :: ToHTML a => a -> [HtmlGenParser a]
-- -- where goal would just be to take a specific collection of patterns then grab all in one go
-- -- * this would neeeeed arrows due to garbage collection and size of said garbage




-- | ON URL-File Management:
-- |
-- | 1 File: Top part, if it exists is Search And/Or Browse Links
-- |         ->> below is the basic pages file tree

finish :: [String] -> [String] -> String
finish [] _ = ""
finish (x:xs) (y:ys) =  x <> "=" <> y <> "&" <> finish xs ys

finish' :: ([String], [String]) -> Int -> String -> String
finish' (paramA:paramAs, paramB:paramBs) idx term = case idx of
  0 -> paramA <> "=" <> term <> "&" <> finish paramAs paramBs
  _ -> paramA <> "=" <> paramB <> "&" <> finish' (paramAs, paramBs) (idx-1) term 

genSerchStrm :: String -> ([String], [String]) -> [Text]
genSerchStrm searchTerm params = fmap (pack . reverse) $ go searchTerm start params --(start, params) 
  where
    start = (length . fst $ params) - 1

    go :: String -> Int -> ([String], [String]) -> [String]
    go term idx params = if idx == -1 then [] else finish' params idx term : go term (idx-1) params 

    -- 1 use of idx sets the next eval'd string , the other subtracts until its at that piece and
    -- sees 0 thus evaluating the logical placement 
    
    
    -- go _ _ ([],_) _ = []
    -- -- go term 0 (paramA:paramAs, paramB:paramBs) (stateIdx, state1) =
    -- --   --in this case, idx will always be 0
    -- --   (paramA <> "=" <> term <> "&" <> finish paramAs paramBs) : go term (stateIdx+1) state1 (stateIdx,state1)
           
    -- go term idx params = 
      

      -- Seems like I need a go-like function which is forced to return Text/String not [Text]
       -- would need idx, params, term
       -- would literally be iter1 <> iter2 


    -- so if they havent hit pattern: idx==0 yet but no more left to pull and create then the
    -- idx must exceed the length of the list 

allElems :: [String]
allElems = ["this is a placeholder for describing all html tags as a type system"]

-- | input has attr: type="" which determines value type for form url
-- | Attrs that I care about really are refs@( name || id ) , (Maybe) value, type,
inputElems :: Maybe [String] 
inputElems = Just [ "input"
                  , "select"
                  , "textarea"
                  , "button"
                  , "datalist"
                  , "meter"
                  , "progress"
                  , "radio" ]

-- Another idea is a type family for Elem tags that allows for certain functions to specify
-- if they only work with a subset of tags ; if it works with all : it specifies the type family
-- as the input arg

inputTypes :: [String]
inputTypes = ["button", "checkbox", "color", "date", "datetime-local", "email", "file", "hidden", "image", "month", "number", "password", "radio", "range", "reset", "search", "submit", "tel", "text", "week", "time", "url"]


    -- <input type="button">
    -- <input type="checkbox">
    -- <input type="color">
    -- <input type="date">
    -- <input type="datetime-local">
    -- <input type="email">
    -- <input type="file">
    -- <input type="hidden">
    -- <input type="image">
    -- <input type="month">
    -- <input type="number">
    -- <input type="password">
    -- <input type="radio">
    -- <input type="range">
    -- <input type="reset">
    -- <input type="search"> --> Is just a text box
    -- <input type="submit">
    -- <input type="tel">
    -- <input type="text">
    -- <input type="time">
    -- <input type="url">
    -- <input type="week">
             --src: https://www.w3schools.com/html/html_form_input_types.asp

  -- How are buttons handled?

  -- How should we handle cases like file? Should we specify value = Nothing?

-- | Seems sepElems is not yet perfected for the whole scope of HTML Possibilities but
-- | All functions that pull from (,,,) / 4[]


-- | Could we just use parseElemHead here?
  -- probably gonna be waaaaay more efficient with self-closing tags and I dont think there's
  -- any relevance of the (maybe innerText)
-- | && shouldnt this specify certain attrs? 145


innerFormParser :: Stream s m Char => ParsecT s u m [Elem' String]
innerFormParser = do
  x <- findNaive (elemParser inputElems Nothing [])
  case x of
    Just listOfElems -> return listOfElems
    Nothing -> undefined --return []
    
-- | This would likely be way more efficient if we parsed as TreeHTML then "trimmed" down to
-- | what we want but it could also be less efficient 
-- | would be fed from some initial parser
buildFormUrlEndings :: String -> Elem' a -> Either String ([Text], [Text])
buildFormUrlEndings searchTerm formElem = case elTag formElem of

  -- What about the base tag for the website that this ending will get applied to?
  -- ie how will we determine that?
    --  We can get this from the western database site

  -- Some may have query parameter of accountid=Int

  -- Would this logic work? :
    -- get redirected base URL
    -- concat this + whatever paths & params
  
  "form" -> do --Either a b is the Monad
    case fmap sepElems (parse innerFormParser "" (innerText' formElem)) of
      Left err -> Left $ show err
      Right (basic, textInput, variable, radio) ->
        let
          actionAttr :: Text
          actionAttr = (pack . fromJust . (Map.lookup "action") . attrs) formElem
          basic' :: Text
          basic' = actionAttr <> (createBasicQKVPairs basic)
          vars :: Map Namespace [Option] 
          vars = fromList (mkOptMaps variable)
          radio' :: Map Namespace [Option]
          radio' = iterRadios radio empty 
          subsetVariables = union vars radio'
          subsetVars = [ basic' <> x | x <- buildSearchUrlSubsets subsetVariables]
          textInput' = ( ((fromMaybe "") . (Map.lookup "name") . attrs) <$> textInput
                       , fmap (findWithDefault "" "value" . attrs) textInput)
          textInput'' = fmap (basic' <>) (genSerchStrm searchTerm textInput') -- :: [String]
        in return (subsetVars, textInput'')
        --  at macro, can build then try Url ... case Fail -> build, try next

  _ -> Left $ "This only has utility on form elements"
    
--     let searchTermParam = cycle (with = searchTerm, txtElems !! 0 ) --in fail case -> cycle to next, this logic can be performed easily anywhere

--     try: mkReq searchTermParam (splitup then created elemsTotal :: [Text]) <- may write to tmp file so that we can easily perform concurrent scraping of diff websites 
--     case fail -> newSearchTermParam ; success -> go for some amount of times 

--
getAttrVal :: String -> Elem' a -> String 
getAttrVal name formElem = (fromJust . (Map.lookup name) . attrs) formElem

buildSearchUrlSubsets :: Map Namespace [Option] -> [Text]
buildSearchUrlSubsets mappy = singlefOp "" (toList mappy)

-- <> of base + set pairs
-- | Note: this url may fail

-- | SHOULD I? create a list of baseUrls, where each could fail and if so, next
-- buildBaseFormUrl baseUrl generalSearchTermToSearchForTextbox ((Elem'{..}):elems)
  --  elem == "textarea" = genSearchTermTBox
  -- plus when input type attribute is text 
  --  elem == "button" = ""

-- baseFormUrlVariants :: a -> [Url]
                          --- basic || textinput || variable || Radio input-type

sepElems :: [Elem' a] -> ([Elem' a], [Elem' a], [Elem' a], [Elem' a])
sepElems elems = go elems ([], [], [], [])
  where go [] endState = endState
        go (elem:elems') (a,b,c,d)
          | elTag elem == "select" || elTag elem == "datalist" = go elems' (a, b, elem : c, d)
          | elTag elem == "textarea" = go elems' (a, elem: b, c, d)
          | elTag elem == "input" = 
              case Map.lookup "type" (attrs elem) of
                Just "text" -> go elems' (a, elem : b, c, d)
                Just "radio" -> go elems' (a, b, c, elem : d)
                _ -> go elems' (elem : a, b, c, d)
                -- "hidden" ^^ 
                
          | elTag elem == "button" = go elems (elem : a, b, c, d)
          -- should this change if the button is of type submit?

 -- sepElems' :: [ElemHead] -> ([ElemHead], [ElemHead], [ElemHead], [ElemHead])
-- sepElems' heads = go heads ([],[],[],[])
--   where go :: [ElemHead]
--           -> ([ElemHead],  [ElemHead], [ElemHead], [ElemHead])
--           -> ([ElemHead],  [ElemHead], [ElemHead], [ElemHead]) 
--         go [] endState = endState
--         go ((tag, attrs):elHeads) (a,b,c,d)
--           | tag == "select" || tag == "datalist" = go elHeads (a, b, elem : c, d)
--           | tag == "textarea" = go elHeads (a, elem: b, c, d)
--           | tag == "input" = 
--               case Map.lookup "type" (attrs elem) of
--                 Just "text" -> go elHeads (a, elem : b, c, d)
--                 Just "radio" -> go elHeads (a, b, c, elem : d)
--                 _ -> go elHeads (elem : a, b, c, d)
--                 -- "hidden" ^^ 
                
--           | tag == "button" = go elHeads (elem : a, b, c, d)


-- |for variable elems, this will be a case where we need to parse inner text again for the option elements

-- | This likely has a far better implementation although im honestly not sure how much "overhead"
-- | would be removed by not using parser again
-- | Likely tho; -> best option always is 1 pass parsing
  -- every time we call "parse" I believe its equivalent to parse str where str = strFullHtml ++ strInnerHtmlSpec
  -- where all input is consumed and each index has N number of (->Boolean) functions as options thru <|>
mkOptMapSingle :: SelectElem a -> Maybe (Namespace, [Option])
mkOptMapSingle elem = --confirm is "select" then parse options ; \case (options) of Just a -> fmap pack a)
  if elTag elem /= "select" --can expand to elemOf 
  then Nothing
  else 
    case parse formOptionsParser "bsSourceName" (innerText' elem) of
      Left _ -> Nothing -- dont care why it failed, just that it did
      Right a -> Just $ (pack $ fromJust $ Map.lookup "id" (attrs elem), fmap pack a)

  
  -- (fromJust $ Map.lookup "name" (attrs elem)
  -- , case parse formOptionsParser "bsSourceName" (innerHtmlFull elem) of
  --     Left _ -> Nothing -- dont care why it failed, just that it did
  --     Right a -> Just $ fmap pack a)
  
-- | above should check if : el elem == "select"


mkOptMaps :: [SelectElem a] -> [(Namespace, [Option])]  --- -> Map
mkOptMaps [] = [] 
mkOptMaps (elem:elems) = case mkOptMapSingle elem of
                           Just a -> a : mkOptMaps elems
                           Nothing -> mkOptMaps elems 


--func idea:
applyFailStream :: (a -> Bool) -> [a] -> [a]
applyFailStream = undefined

optionElemsPat :: Maybe [String]
optionElemsPat = Just ("option":[])




-- this can safely assume that no inner text exists
formOptionsParser :: Stream s m Char => ParsecT s u m [String]
formOptionsParser = do
  (elems :: [Elem' String]) <- many (elemParser optionElemsPat Nothing []) -- :: (Stream s m Char, ShowHTML a) => ParsecT s u m [Elem' a]
  -- type Elem'
  let
    attrssToFVals :: [Map String String] -> [String]
    attrssToFVals [] = []
    attrssToFVals (attrs:attrss) = case Map.lookup "value" attrs of
                                       Just a -> a : attrssToFVals attrss
                                       Nothing -> "" : attrssToFVals attrss

  -- fmap attrs elem :: [Map k a]
  
  return $ attrssToFVals (fmap attrs elems)

--gonna run on innerHTMLFull elem --> [(Name, [Option])]

   -- 1:2:3 (:) 4:5:6 IFF (:)[] 

-- | I have to use (<>) to deal with the fact that the end of the list would look the exact same as an "inner end"
-- | but this doesnt mean that I will have infinite listing ending up with something like [[[[[[[[a]]]]]]]]

-- | Although, ^ this may be more likely a problem if using cons, (:) since it creates a more complex type
-- | which works fine at the last iteration where strings are thus being operated like so

    -- string : (f x --rec--> [string])
  
-- | Meaning that at whateverrr depth we will be able to (<>) the [string]

   -- [string] <> [string] --> [string]

-- | Meaning this will work for any depth of tree

toStr2 :: Text -> Namespace -> Option -> Text
toStr2 txt name value = txt <> name <> "=" <> value <> "&" 

-- | singlefOp can be thought of as a state carrying function which evaluates to N number of
-- | functions which evaluate to a list of the expression of toStr2 

singlefOp :: Text -> [(Namespace, [Option])] -> [Text]
-- singlefOp :: String -> Map Namespace [Option] -> [String]
singlefOp str ((namespace, []):[]) = [] -- allows for => x : singlefOp
-- | Above may be useless/repetitive to just below 
singlefOp str ((namespace, xs):[]) = [ toStr2 str namespace x | x <- xs ]
-- | This ^ creates actual strings then recurses with (:)

-- | Creates specifically last branch ^^
-- levels to go (below) no levels to go (up)
singlefOp str ((namespace, x:[]):levels) = singlefOp (toStr2 str namespace x) levels
singlefOp str ((namespace, (x:xs)):levels) =
  singlefOp (toStr2 str namespace x) levels <> singlefOp str ((namespace, xs):levels) 
                                      --still same namespace but next iteration of namespace
                                      --with new value


-- | So maybe f takes a state parameter that is determined by the higher-level-set value


-- Could also be that we'll need slightly different unpacking for going across vs in
  --and that one could end up as an mconcat / <>

-- both in || to side deal with leftover params
  

-- | SO add the results of f :: ItemOfCurrentList -> [ItemOCL] -> [String]
  
-- | for basic
createBasicQKVPair :: Elem' a -> Text
createBasicQKVPair elem = (pack (fromJust (Map.lookup "name" (attrs elem))))
                          <> "=" <> (pack (findWithDefault "" "value" (attrs elem)))
                          

createBasicQKVPairs :: [Elem' a] -> Text
createBasicQKVPairs [] = ""
createBasicQKVPairs (elem:elems) = createBasicQKVPair elem <> createBasicQKVPairs elems

-- | NOTE: this below function sucks createQKVP...Term
-- | Instead i will do a cycle pattern that controls it 
                    
-- |case request of
-- |  Fail -> move first query param to end of list/string and move the search term
-- |  Success -> well then do ya thang shawty



-- | NOT REFERENCED
createQKVPairWithTerm :: Text -> [Elem' a] -> [Text]
createQKVPairWithTerm _ [] = []
createQKVPairWithTerm term (next:tInputElems) =
  (pack . fromJust) (Map.lookup "name" (attrs next)) <> "=" <> term : createQKVPairWithTerm term tInputElems
--   :


-- I : Attrs ~ Map Attr{especially "name", "value"} AttrValue

-- O : [(Name, [Options]), (Name2, [Options2]) ... ]

iterRadios :: [Elem' a] -> Map Namespace [Option] -> Map Namespace [Option]
iterRadios [] finalOptsMap = finalOptsMap 
iterRadios (elem:elems) startingOptsMap =
  iterRadios elems (lookupOrInsertName (attrs elem) startingOptsMap)   


-- can even just pass empty func :: Map k a as starting optsMap 
lookupOrInsertName :: Attrs -> Map Namespace [Option] -> Map Namespace [Option] 
lookupOrInsertName attrsMap optsMap =
  case Map.lookup (pack $ fromMaybe "" (Map.lookup "name" attrsMap)) optsMap of
    Just list ->
      adjust ((pack (fromMaybe "" (Map.lookup "value" attrsMap))) :) (pack (fromMaybe "" (Map.lookup "name" attrsMap))) optsMap
       
    Nothing ->
      insert (pack (fromMaybe "" (Map.lookup "name" attrsMap))) [] optsMap
  -- where
  --   k = 

-- | NOT REFERENCED
-- groupRadio :: [Elem'] -> Map Name [Option]
-- groupRadio (elem:elems) = go elems []
--   where go [] listOut = listOut 
--         go (elem: elems) listOut = go' elem (opt:listOut)

--         go' elem (opt:listOut) = case (lookup "name" (attrs elem)) listOut of
--                 True ->
--                   fmap (\(a,b) (a',b') -> if a == a'
--                                           then (a, b' <> b)
--                                           else id) getTheValue (attrs elem)  -- :: Map String String
--                 False -> --add param/name to listOut
--                   ((lookup "name" (attrs elem), [lookup "value" (attrs elem)]) : listOut)



-- findListingElems 


-- listLikeElems = ["blockquote", "div", "dl" -> "dt" , "li", "p", "section", "article"
                -- , "listing", "col","colgroup","table" || "tbody" || "td" ||| "tr", "a", "span", "div",                ,"small"


-- likelyListElems = ["li", "tr", "dt", "div", "section", "article", "listing", "a", "p", ("td", "col"), "small"]



-- | Just an arbitrarily set number of elements
-- | Could also see if select-like elems count == 0
searchIsBigorSmall :: [Elem' a] -> Bool
searchIsBigorSmall = undefined


-- | Just use substring
-- | Will it be this page tho? Or a sort of weird response


-- | Should use findSomeSameEl probably
findPagination :: ParsecT s u m (TreeHTML a)
findPagination = undefined


structuredBrowsingLinksExist :: ParsecT s u m (TreeHTML a)
structuredBrowsingLinksExist = undefined
-- perhaps we should have some recursive func that scopes in and tries to find the same elem+attrs
-- for each level of nesting, to see if at any point, theres some level of repetition potentially
-- indicating that this is a list of similar things

--------------------------------------------------------------------------------------------------------------------
-------------------Pagination-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- uriParser can literally just be parser from Text.URI



-- 1) Look through a parsed tree for href =
-- 2) scan text for URI.parser ; p  

getValidHrefs :: Stream s m Char => String -> ParsecT s u m (Maybe [String])
getValidHrefs baseUrl = findNaive (getValidHref baseUrl)

getValidHref :: Stream s m Char => String ->  ParsecT s u m String
getValidHref baseUrl = hrefParser' (isPrefixOf baseUrl)
  -- where
    -- cheapShit :: String -> String -> Bool 
    -- cheapShit baseU str = isPrefixOf baseU str

    
      -- case parse URI.parser( ) "" str of
      --   -- this will work tho 
      --   Right uri -> True
      --   Left _ -> False 

    -- orCheapShit str = isPrefixOf "https://" str 

  -- get all mutually exclusive <a> with href == actualUriParser | validURI 
  
derivePagination :: Stream s m Char => String -> ParsecT s u m UrlPagination
derivePagination baseUrl = do
  --should also check attrs for if link exists in the paginationElements head href attr
  -- in the case of x,y,z having above case, it should be consed to be first try
  let
    f' x = (Map.lookup "href" . attrs) x  
    g x y z = if f' x /= Nothing && f' y /= Nothing && f' z /= Nothing
              then (fromJust $ f' x, fromJust $ f' y, fromJust $ f' z)
              else ("", "", "")
  
  (x,y,z) <- paginationElements
  -- note: this is a case of applicative parsing being more optimal

  let
    x' = getPgtnLks x baseUrl
    y' = getPgtnLks y baseUrl
    z' = getPgtnLks z baseUrl


  -- x' <- getPgtnLks x baseUrl
  -- y' <- getPgtnLks y baseUrl
  -- z' <- getPgtnLks z baseUrl
  let
    f :: (String, String, String) -> Maybe UrlPagination
    f (x,y,z) = funcyAf "" x y z  
  case tillWeGetItRight f ((g x y z) : (triZip x' y' z')) of
    Nothing -> parserZero
    Just a -> return a

triZip :: [a] -> [b] -> [c] -> [(a,b,c)]
triZip [] _ _ = []
triZip _ [] _ = []
triZip _ _ [] = []
triZip (a:as) (b:bs) (c:cs) = (a,b,c) : triZip as bs cs

tillWeGetItRight :: (a -> Maybe b) -> [a] -> Maybe b
tillWeGetItRight _ [] = Nothing -- but we got it wrooong 
tillWeGetItRight f (x:xs) = case f x of
                              Just a -> Just a
                              _ -> tillWeGetItRight f xs

funcyAf :: String -> String -> String -> String -> Maybe UrlPagination
funcyAf _ [] _ _ = Nothing
funcyAf _ _ [] _ = Nothing
funcyAf _ _ _ [] = Nothing
funcyAf pre (x:xs) (y:ys) (z:zs) =
  if x == y
  then
    -- Pred: x y ==
    if x == z
    then funcyAf (pre <> (x:[])) xs ys zs -- only case that recurses
         -- could also just reverse at end 
    else
      Nothing
  else
    if y == z
    then Nothing
    else 
      if y == z
      then Nothing
      else
        if (digitToInt y - digitToInt x) - (digitToInt z - digitToInt y) == 0
        then

          case xs == ys && xs == zs of
            True -> Just $ UrlPagination pre 2 xs
            False -> Nothing 
             -- we know the index at which the page, paginates
             -- we have 3 pieces
             -- pre <> pgNum <> post == url
        else Nothing 
      

-- Need to stretch to handling finding many 
getPgtnLks :: ElementRep e => e b -> String -> [String]
getPgtnLks elem baseUrl =
  case (parse (getValidHrefs baseUrl) "" (innerText' elem)) of
    Left _ ->
      case headHref of
        Just a -> a : [] 
        Nothing -> []
      -- return [] 

    Right (Just urlList) ->
      case headHref of
        Just a -> a : urlList
        Nothing ->  urlList 
  where
    headHref :: Maybe String
    headHref = (Map.lookup "href" (attrs elem)) >>= maybeUsefulUrl baseUrl 


    --   -- return urlList 

    -- Right (Nothing) ->
    --   -- this is exactly Left _ 


  
  -- do


  
  -- x2 <- case (parse (findNaive getValidHrefs) "" (innerText' elem)) of
  --         Left _ -> return [] 
  --         Right (Just urlList) -> return urlList 
  --         Right (Nothing) -> return []

  -- -- Container elem's link is first in list
  -- case headHref of
  --   Nothing -> x2
  --   Just a -> return (a:x2)


 -- case :: Maybe [x]
 -- headhref :: Maybe a

 -- (:) <$> case <*> headHref


-- length 3 
paginationElements :: Stream s m Char => ParsecT s u m (Elem' String, Elem' String, Elem' String)
paginationElements = do 
  let
    attrs' e = (fmap . fmap) Just $ Map.toList (attrs e)
    el' e = Just $ [elTag e]
    strip (Left e) = e
    strip (Right e) = e
  
  x <- elemParser Nothing (Just $ string "1") [] 
  -- This could be different from option 1 but still contain 2, validly 
  y <- Right <$> elemParser (el' x) (Just $ string "2") (attrs' x)
       <|> Left <$> elemParser Nothing (Just $ string "2") []
       
  z <- case y of
         Right elemnt -> elemParser (el' elemnt) (Just $ string "3") (attrs' x)
         Left elemnt' -> do
           -- check for the next two to see if same struct three times in a row
           z1 <- elemParser (el' elemnt') (Just $ string "3") (attrs' elemnt')
           z2 <- elemParser (el' elemnt') (Just $ string "4") (attrs' elemnt')
           return z1
  
  return (x,strip y,z)



                       
catEithers :: [Either a b] -> [b]
catEithers [] = []
catEithers (x:xs) = case x of
                      Left err -> catEithers xs
                      Right a -> a : catEithers xs

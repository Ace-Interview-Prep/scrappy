{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module BuildActions where


import Replace.Megaparsec (findAll)

import Elem.SimpleElemParser (elemParser)
-- import Elem.ElemHeadParse 
import Elem.Types (TreeHTML, Elem, Elem'(..), ElemHead, ElementRep, Attrs, ShowHTML, innerText', elTag, attrs, UrlPagination(..), matches', showH)
import Elem.ElemHeadParse (hrefParser', hrefParser, attrsParser, parseOpeningTag)
-- import Links (Link, Option, Namespace, UrlPagination(..), maybeUsefulUrl)

import Links (maybeUsefulUrl, Url )
import Find (findNaive, findSomeHTMLNaive)

import Network.HTTP.Client (Request, queryString, method, parseRequest)
import Network.HTTP.Types.Method (Method, methodPost, methodGet)
import Control.Monad.Catch (MonadThrow)
import Text.Parsec (ParsecT, ParseError, Stream, many, parse, string, (<|>), parserZero, try)
import Text.Parsec.Error (Message (Message))
import Data.Either (fromRight)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Map as Map (Map, insert, adjust, findWithDefault, toList, fromList, lookup, union, empty, insertWith)
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
                      


getContainedUrls :: Elem' a -> Maybe [String]
getContainedUrls e = findSomeHTMLNaive hrefParser (innerText' e)




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

-- | finish is a simplified version of finish' that doesnt need to care about idx, just writes queryString variant
finish :: [String] -> [String] -> String
finish [] _ = ""
finish (x:xs) (y:ys) = "&" <> x <> "=" <> y <> finish xs ys

finish' :: ([String], [String]) -> Int -> String -> String
finish' (paramA:paramAs, paramB:paramBs) idx term = case idx of
  -- SO we are at the right index if 0 
  0 -> "&" <> paramA <> "=" <> term <> finish paramAs paramBs
  _ -> "&" <> paramA <> "=" <> paramB <> finish' (paramAs, paramBs) (idx-1) term 




finish'' :: [Text] -> [Text] -> QueryString
finish'' [] _ = []
finish'' (x:xs) (y:ys) = (x, y) : finish'' xs ys 
  --"&" <> x <> "=" <> y <> finish xs ys

finish''' :: ([Text], [Text]) -> Int -> Text -> QueryString
finish''' (paramA:paramAs, paramB:paramBs) idx term = case idx of
  0 -> (paramA, term) : finish'' paramAs paramBs 
    -- "&" <> paramA <> "=" <> term <> finish'' paramAs paramBs
  _ -> (paramA, paramB) : finish''' (paramAs, paramBs) (idx - 1) term
    -- "&" <> paramA <> "=" <> paramB <> finish''' (paramAs, paramBs) (idx-1) term 



-- | Basically just calls finish' with starting state 
genTInputOpts' :: Text -> ([Text], [Text]) -> [QueryString]
genTInputOpts' searchTerm params =
  if (length $ snd params) /= (length $ fst params) 
  then
    undefined
  else
    reverse $ go searchTerm start params --(start, params) 
  where
    start = (length . fst $ params) - 1
    go :: Text -> Int -> ([Text], [Text]) -> [QueryString]
    go term idx params = if idx == -1 then [] else finish''' params idx term : go term (idx-1) params 



-- | Basically just calls finish' with starting state 
genSerchStrm :: String -> ([String], [String]) -> [Text]
genSerchStrm searchTerm params =
  if (length $ snd params) /= (length $ fst params) 
  then
    undefined
  else
    reverse $ fmap (pack ) $ go searchTerm start params --(start, params) 
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

searchForm :: Stream s m Char => ParsecT s u m (Elem' String)
searchForm = do
  let 
    f = (try $ string "search") <|> (try $ string "Search")
  e <- elemParser (Just ["form"]) (Just $ f) []
  if length (matches' e) < 3
    then parserZero
    else return e


-- both Option and Value ~ Text
data InputElem = Radio Namespace Option | SelectElem (Namespace, [Option]) | Basic Namespace (Maybe Option)
               deriving Show
                                                              -- Maybe distinguishes between basic
-- | SO:

  -- Radio case is also "input" elem tag

-- (radio, variable, (basic, tInput))

-- structure based on filtering steps based on Constructors

-- - Instead of searchForm as arg, will be filtered, parsed form including sorted input elems 


instance ShowHTML InputElem where
  showH = show -- TEMPORARY


data ParsedForm = ParsedForm Action Method [InputElem]


-- f :: ParsedForm -> FilledForm


-- basic, textInput, variable, radio
                            -- | NOT READY               READY AS IS
     
-- | (Radio | Var | Basic | TInput)
type SepdStruct = ([(Namespace, Option)], [(Namespace, [Option])], [(Namespace, Option)], [Namespace])



sepElems' :: [InputElem] -> SepdStruct -> SepdStruct
sepElems' (elem:elems) (a,b,c,d) = case elem of
  Radio nom opt -> sepElems' elems ((nom, opt) : a, b, c, d)
  SelectElem selEl -> sepElems' elems (a, selEl : b, c, d)
  Basic namesp (Just opt) -> sepElems' elems (a, b, (namesp, opt) : c, d) 
  Basic namesp (Nothing) -> sepElems' elems (a, b, c, namesp : d)


  -- elems = ( filter fBasic elems
  --                 , filter fTextInp elems
  --                 , filter fVar elems
  --                 , filter fRadio elems
  --                 )
  -- where
  --   fBasic e =
  --     (elTag e == "input")
  --     && (let typ = fromJust $ Map.lookup "type" (attrs e) in not $ elem typ ["text", "radio"])

  --   fTextInp e = ( elTag e == "textarea" )
  --                || (elTag e == "input" && (fromJust (Map.lookup "type" (attrs e)) == "text"))

  --   fVar e = elem (elTag e) ["select", "datalist"]

  --   fRadio e = elTag e == "input" && (fromJust (Map.lookup "type" (attrs e)) == "radio")
     



formElem :: Stream s m Char => ParsecT s u m ParsedForm
formElem = do
  Elem' _ as matches _  <- elemParser (Just ["form"]) (Just inputElem) []
  return $ ParsedForm (getAction as) (getMethod as) matches
  where
    getAction = pack . fromJust . (Map.lookup "action")
    getMethod as = case fromJust ((Map.lookup "method") as) of
                  "get" -> methodGet
                  "post" -> methodPost

inputElem :: Stream s m Char => ParsecT s u m InputElem
inputElem = radioBasic <|> selectEl 

-- checks el tag then returns either radio or 
radioBasic :: Stream s m Char => ParsecT s u m InputElem
radioBasic = do
  (_, attrs) <- parseOpeningTag (Just ["input"]) [] 
  if (fromMaybe "" $ Map.lookup "type" attrs) == "radio"
    then return $ Radio (pack . fromJust $ Map.lookup "name" attrs) (pack . fromJust $ Map.lookup "value" attrs)
    else return $ Basic (pack . fromJust $ Map.lookup "name" attrs) (fmap pack $ Map.lookup "value" attrs)

-- SelectElem Name [Option]

-- | formRaw <- elemParser (Just ["form"]) (Just $ inputElem) [] 
-- | return ParsedForm Action $ build (matches formRaw)... 

selectEl :: Stream s m Char => ParsecT s u m InputElem
selectEl = do
  e <- elemParser (Just ["select", "datalist"]) (Just optionParser) []
  return $ SelectElem ((pack . fromJust . (Map.lookup "name") . attrs $ e), (fmap pack $ matches' e))
  where
    name e = pack . fromJust . (Map.lookup "name") . attrs $ e
    opts e = fmap pack (matches' e)

-- parseOpeningTag (Just ["option"])

optionParser :: Stream s m Char => ParsecT s u m String
optionParser = do
  (_, a) <- parseOpeningTag (Just ["option"]) []
  return $ (fromJust . (Map.lookup "value")) a


  -- _ <- string "<option "
  -- x <- fmap (fromRight (parserZero :: ParsecT s u m ) (attrsParser [])
  -- return $ (fromJust . (Map.lookup "value")) x
               
  
  


-- innerFormParser' :: Stream s m Char => ParsecT s u m [InputElem]
-- innerFormParser' = do
--   x <- findNaive (inputElemParser inputElems Nothing [])
--   case x of
--     Just listOfElems -> return listOfElems
--     Nothing -> undefined --return []

innerFormParser :: Stream s m Char => ParsecT s u m [Elem' String]
innerFormParser = do
  x <- findNaive (elemParser inputElems Nothing [])
  case x of
    Just listOfElems -> return listOfElems
    Nothing -> undefined --return []

-- type SearchQuery = Text
type TInputOpt = QueryString
type Action = Text
type BaseUrl = Url 
data FilledForm = FilledForm { baseUrl :: Url
                             , reqMethod :: Method
                             , searchTerm :: Term
                             , actnAttr :: Action
                             , textInputOpts :: [TInputOpt]
                             , qStringVariants :: [QueryString]
                             } deriving Show 
-- | If I instead just pass a baseUrl to buildFormSummary then I can return as:


-- Searches ( PerformSearch  _    _  [FilledForm]  _ )
-- Searches [PeformSearch _    _   FilledForm _ ] 


data SearchSumm = SearchSumm Term Method Action [TInputOpt] [QueryString] 


--  -- | then for resPapScrap, ((\Form' term reqs -> PerformSearch term reqs stuff) . mkSearch)
--  -- | Then voila we have (with appropriate auth wrapping and fmapping to all genres
--         -- | we have the next SiteState!!

-- | IMPLEMENTATION
-- | *** MAIN of forms / search
-- mkSearchEnum :: Url -> Elem' a -> Term -> Either FormError SearchEnum
-- mkSearchEnum baseUrl form term = do 
  -- (FilledForm baseUrl reqMethod term aAttr searchQueriesTextOpts searchQueriesEnumd) <- buildFormSummary baseUrl term form 
  -- mkSearch' baseUrl reqMethod term aAttr (head searchQueriesTextOpts) searchQueriesEnumd

type Term = String 
data SearchEnum = SearchEnum Term [Request]
mkSearch' :: Url -> Method -> Term -> Action -> QueryString -> [QueryString] -> Either FormError SearchEnum 
mkSearch' baseUrl reqMethod term action textIOpt queriesEnumd = do
  case parseRequest $ baseUrl <> (unpack action) of
    Right req -> do
      let
        req2 = req { method = reqMethod }
      return $ SearchEnum term
        $ fmap (\query -> req2 { queryString = encodeUtf8 $ showQString (textIOpt <> query) }) queriesEnumd 
    Left _ -> Left UrlError 
  
showQString :: [(Namespace, Option)] -> Text
showQString xs = f (head xs) <> showQSInner (tail xs) 
  where
    f (n, v) = n <> "=" <> v
    showQSInner [] = ""
    showQSInner (y:ys) = "&" <> (fst y) <> "=" <> (snd y) <> (showQSInner ys)
    

-- -- | This function's goal is to give the full callable, valid paths that get this genre some results
-- mkGenredSearch :: Url -> Elem' a -> Genre -> Either FormError Search -- (Genre, [SearchQuery])
-- mkGenredSearch baseUrl formE genre = do
--   (method, queries) <- buildFullSearchQuerys genre formE
--   return $ Search [Request] --  method genre (fmap (baseUrl <>) queries)

  -- fmap (genre,) $ (fmap . fmap) (baseUrl <>) (buildFullSearchQuerys genre formE)
  -- fmap (Search _ genre) $ (fmap . fmap) (baseUrl <>) $ (method,queries) = buildFullSearchQuerys genre formE)

  
type SearchQuery = Query 
type Query = Text 

data FormError = InvalidElement
               | ParsecError ParseError
               | UrlError
               deriving Show
-- -- | Note that buildFormUrlEndings gives a list of different configs of text inputs for a single search term
-- -- | as well as the in order list of selected options -> So really this function is  just a means to a long
-- -- | list of results for a given query/search term

-- -- | In future we will add fallback in the case of a very useless text box like say if we found the author input
-- -- | and erroneously used it as (title OR anywhere) text input 
-- buildFullSearchQuerys :: Term -> Elem' a -> Either FormError (RequestMethod, [SearchQuery])
-- buildFullSearchQuerys genre formE = case buildFormSummary genre formE of
--   Right (Form method actionAttr (textInput:_) subsets) ->
--      Right $ (method, fmap ((unpack textInput <>) . unpack) subsets)
--   Left str -> Left $ OtherError str --undefined -- Left OtherError str

  -- (Form method actionAttr (textInput:_) subsets) <- buildFormSummary genre formE
  -- return 



         


-- -- IN Respapscrap

-- FormSummary -> Search

-- data Search = Search RequestMethod [BaseUrl] [QueryString]
-- data Search' = Search [Request] =<< querystring, base url and method `from` Form ...

-- data S = S BaseUrl RequestMethod [Queries]


-- formToSearch' (Form reqMethod action   = do


                  
-- WAIT!!!!!!!!!!!!





                  -- Is it really [BaseUrl] ? or just baseUrl .... from state so: method <+> queries as Search






-- type FormInput a = Elem' a  

-- buildFormSummary' :: Url -> String -> [FormInput] -> FilledForm 
-- buildFormSummary baseUrl searchTerm inputs =
  -- let
    -- (basic, textInput, variable, radio) = sepElems inputs
    -- method e = case (fromJust $ ((Map.lookup "method") . attrs) e) of
      -- "get" -> methodGet
      -- "post" -> methodPost 
    -- subPaths = searchTermSubPaths (mkSubsetVars variable radio) (mkBasicParams' basic)
    -- textInput' = ( (pack . (fromMaybe "") . (Map.lookup "name") . attrs) <$> textInput
                 -- , fmap (pack . (findWithDefault "" "value") . attrs) textInput)
    -- textInputOpts = (genTInputOpts' (pack searchTerm) textInput') -- :: [String]
  -- in FilledForm baseUrl (method formElem) searchTerm (actionAttr formElem) textInputOpts subPaths


mkFilledForm :: Url -> String -> Elem' a -> ([Elem' a], [Elem' a], [Elem' a], [Elem' a]) -> FilledForm
mkFilledForm baseUrl searchTerm formElem (basic, textInput, variable, radio) = 
  let
    method e = case (fromJust $ ((Map.lookup "method") . attrs) e) of
     "get" -> methodGet
     "post" -> methodPost
    
    basic' = mkBasicParams' basic
    radio' = radiosToMap (fmap mkNameVal radio)
    variable' = mkOptMaps' variable
    subPaths' = buildSearchUrlSubsets (union variable' radio')
    textInputOpts' = fmap (basic' <>) (genTInputOpts' (pack searchTerm) textInput') 
    
    -- subPaths = searchTermSubPaths (mkSubsetVars variable radio) (mkBasicParams' basic)
    textInput' = ( fmap (pack . (fromMaybe "") . (Map.lookup "name") . attrs) textInput
                 , fmap (pack . (findWithDefault "" "value") . attrs) textInput)
    
    textInputOpts = (genTInputOpts' (pack searchTerm) textInput') -- :: [String]
 -- in return (subPaths, textInputOpts)
 in FilledForm baseUrl (method formElem) searchTerm (actionAttr formElem) textInputOpts' subPaths'

getFormInputElems :: Elem' String
                  -> Either FormError ([Elem' String], [Elem' String], [Elem' String], [Elem' String])
getFormInputElems formElem = case elTag formElem of
  "form" ->
    case fmap sepElems $ parse innerFormParser "" (innerText' formElem) of
      Left err -> Left $ ParsecError err
      Right a -> return a
  _ -> Left InvalidElement

--  This would likely be way more efficient if we parsed as TreeHTML then "trimmed" down to
--  what we want but it could also be less efficient 
--  would be fed from some initial parser
buildFormSummary :: Url -> String -> Elem' String -> Either FormError FilledForm
buildFormSummary baseUrl searchTerm formElem = case elTag formElem of
  "form" -> do --Either a b is the Monad
    case fmap sepElems $ parse innerFormParser "" (innerText' formElem) of
      Left err -> Left $ ParsecError err
      Right (basic, textInput, variable, radio) ->
        return $ mkFilledForm baseUrl searchTerm formElem (basic, textInput, variable, radio) 
        -- let
        --   method e = case (fromJust $ ((Map.lookup "method") . attrs) e) of
        --     "get" -> methodGet
        --     "post" -> methodPost 
        --   subPaths = searchTermSubPaths (mkSubsetVars variable radio) (mkBasicParams' basic)
        --   textInput' = ( (pack . (fromMaybe "") . (Map.lookup "name") . attrs) <$> textInput
        --                , fmap (pack . (findWithDefault "" "value") . attrs) textInput)
        --   textInputOpts = (genTInputOpts' (pack searchTerm) textInput') -- :: [String]
        -- -- in return (subPaths, textInputOpts)
        -- in return $ FilledForm baseUrl (method formElem) searchTerm (actionAttr formElem) textInputOpts subPaths
        --  at macro, can build then try Url ... case Fail -> build, try next
  _ -> Left InvalidElement      


-- -- | This would likely be way more efficient if we parsed as TreeHTML then "trimmed" down to
-- -- | what we want but it could also be less efficient 
-- -- | would be fed from some initial parser
-- buildFormUrlEndings :: String -> Elem' a -> Either String ([Text], [Text])
-- buildFormUrlEndings searchTerm formElem = case elTag formElem of

--   -- What about the base tag for the website that this ending will get applied to?
--   -- ie how will we determine that?
--     --  We can get this from the western database site

--   -- Some may have query parameter of accountid=Int

--   -- Would this logic work? :
--     -- get redirected base URL
--     -- concat this + whatever paths & params
  
--   "form" -> do --Either a b is the Monad
--     case fmap sepElems (parse innerFormParser "" (innerText' formElem)) of
--       Left err -> Left $ show err
--       Right (basic, textInput, variable, radio) ->
--         let
--           -- actionAttr :: Text
--           -- actionAttr = (pack . fromJust . (Map.lookup "action") . attrs) formElem
--           -- basic' :: Text
--           -- basic' = actionAttr <> (createBasicQKVPairs basic)
--           -- vars :: Map Namespace [Option] 
--           -- vars = fromList (mkOptMaps variable)
--           -- radio' :: Map Namespace [Option]
--           -- radio' = iterRadios radio empty 
--           -- subsetVariables = union vars radio'
--           -- subsetVars = [ basic' <> x | x <- buildSearchUrlSubsets subsetVariables]
--           subPaths = searchTermSubPaths (mkSubsetVars variable radio) (mkBasicPart (actionAttr formElem) basic)
--           textInput' = ( ((fromMaybe "") . (Map.lookup "name") . attrs) <$> textInput
--                        , fmap (findWithDefault "" "value" . attrs) textInput)
--           -- textInput'' = fmap (basic' <>) (genSerchStrm searchTerm textInput') -- :: [String]
--           textInput'' = (genSerchStrm searchTerm textInput') -- :: [String]

--           -- i think basic got repeated
--           -- textInput'' expected length is 10
--           -- subsetVars length is of range (0, 30000)
--         in return (searchTermSubPaths (subPaths, textInput'')
--         --  at macro, can build then try Url ... case Fail -> build, try next

--   _ -> Left "This only has utility on form elements"


       
--     let searchTermParam = cycle (with = searchTerm, txtElems !! 0 ) --in fail case -> cycle to next, this logic can be performed easily anywhere

--     try: mkReq searchTermParam (splitup then created elemsTotal :: [Text]) <- may write to tmp file so that we can easily perform concurrent scraping of diff websites 
--     case fail -> newSearchTermParam ; success -> go for some amount of times 

--

actionAttr :: Elem' a -> Text
actionAttr formElem = (pack . fromJust . (Map.lookup "action") . attrs) formElem

-- mkBasicPart :: Text -> [Elem' a] -> Text
-- mkBasicPart actionAttr basicEs = actionAttr <> (createBasicQKVPairs basicEs)

mkBasicParams' :: [Elem' a] -> QueryString -- [(Namespace, Option)]
mkBasicParams' = createBasicQKVPairs

mkSubsetVars :: [Elem' a] -> [Elem' a] -> Map Namespace [Option]
mkSubsetVars variable radio = union (fromList (mkOptMaps variable)) (iterRadios radio empty)

searchTermSubPaths :: Map Namespace [Option] -> QueryString -> [QueryString] 
searchTermSubPaths subsetVariables basicPath = fmap (basicPath <>) (buildSearchUrlSubsets subsetVariables)

-- buildSearchUrlSubsets (mkSubsetVars variable radio) 


type QueryString = [(Namespace, Option)] 
-- buildSearchUrlSubsets (mkSubsetVars variable radio) 


getAttrVal :: String -> Elem' a -> String 
getAttrVal name formElem = (fromJust . (Map.lookup name) . attrs) formElem

buildSearchUrlSubsets :: Map Namespace [Option] -> [QueryString]
buildSearchUrlSubsets mappy = singlefOp' mempty (toList mappy) -- I believe "" is just state 

-- <> of base + set pairs
-- | Note: this url may fail

-- | SHOULD I? create a list of baseUrls, where each could fail and if so, next
-- buildBaseFormUrl baseUrl generalSearchTermToSearchForTextbox ((Elem'{..}):elems)
  --  elem == "textarea" = genSearchTermTBox
  -- plus when input type attribute is text 
  --  elem == "button" = ""

-- baseFormUrlVariants :: a -> [Url]
                          --- basic || textinput || variable || Radio input-type

sepElems2 elems = ( filter fBasic elems
                  , filter fTextInp elems
                  , filter fVar elems
                  , filter fRadio elems
                  )
  where
    fBasic e =
      (elTag e == "input")
      && (let typ = fromJust $ Map.lookup "type" (attrs e) in not $ elem typ ["text", "radio"])

    fTextInp e = ( elTag e == "textarea" )
                 || (elTag e == "input" && (fromJust (Map.lookup "type" (attrs e)) == "text"))

    fVar e = elem (elTag e) ["select", "datalist"]

    fRadio e = elTag e == "input" && (fromJust (Map.lookup "type" (attrs e)) == "radio")
    





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

  
  -- (fromJust $ Map.lookup "name" (attrs elem)
  -- , case parse formOptionsParser "bsSourceName" (innerHtmlFull elem) of
  --     Left _ -> Nothing -- dont care why it failed, just that it did
  --     Right a -> Just $ fmap pack a)
  
-- | above should check if : el elem == "select"


-- f :: ParsecT s u m SelectElem


data SelectElem' = SelectElem' Namespace [Option]

mkOptMaps :: [SelectElem a] -> [(Namespace, [Option])]  --- -> Map
mkOptMaps [] = [] 
mkOptMaps (elem:elems) = case mkOptMapSingle elem of
                           Just a -> a : mkOptMaps elems
                           Nothing -> mkOptMaps elems 
 
-- |for variable elems, this will be a case where we need to parse inner text again for the option elements

mkOptMaps' :: [SelectElem a] -> Map Namespace [Option]
mkOptMaps' es = fromList $! catMaybes (fmap mkOptMapSingle es)

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







--func idea:
applyFailStream :: (a -> Bool) -> [a] -> [a]
applyFailStream = undefined

optionElemsPat :: Maybe [String]
optionElemsPat = Just ("option":[])

formOptionsParser :: Stream s m Char => ParsecT s u m [String]
formOptionsParser = (findNaive $ elemParser optionElemsPat (Nothing :: Maybe (ParsecT s u m String)) [])
  >>= return . (fmap (fromJust . Map.lookup "value" . attrs)) . fromJust


-- -- this can safely assume that no inner text exists
-- formOptionsParser :: Stream s m Char => ParsecT s u m [String]
-- formOptionsParser = do
--   (elems :: [Elem' String]) <- many (elemParser optionElemsPat Nothing []) -- :: (Stream s m Char, ShowHTML a) => ParsecT s u m [Elem' a]
--   -- type Elem'
--   -- let
--     -- attrssToFVals :: [Map String String] -> [String]
--     attrssToFVals [] = []
--     attrssToFVals (attrs:attrss) = case Map.lookup "value" attrs of
--                                        Just a -> a : attrssToFVals attrss
--                                        Nothing -> "" : attrssToFVals attrss

--   fmap attrs elem :: [Map k a]
  
--   return $ attrssToFVals (fmap attrs elems)

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

-- | Should always be singleton !!! 
toLis2' :: [(Namespace, Option)] -> Namespace -> Option -> [(Namespace, Option)] 
toLis2' lis namesp opt = lis <> ((namesp, opt) :[]) 

toStr2 :: Text -> Namespace -> Option -> Text
toStr2 txt name value = txt <> "&" <> name <> "=" <> value 

-- | singlefOp can be thought of as a state carrying function which evaluates to N number of
-- | functions which evaluate to a list of the expression of toStr2 

singlefOp' :: [(Namespace, Option)] -> [(Namespace, [Option])] -> [[(Namespace, Option)]] 
singlefOp' lis [] = []
singlefOp' lis (("", _):_) = undefined
singlefOp' lis ((namespace, []):[]) = [] -- allows for => x : singlefOp
singlefOp' lis ((namespace, []):levels) = singlefOp' lis levels --omitting potential path of namespace=""
singlefOp' lis ((namespace, x:[]):levels) = singlefOp' (toLis2' lis namespace x) levels
singlefOp' lis ((namespace, xs):[]) = [ toLis2' lis namespace x | x <- xs ]
-- Main: 
singlefOp' lis ((namespace, (x:xs)):levels) = -- Divide ->
  singlefOp' (toLis2' lis namespace x) levels -- Branch 1 
  <> singlefOp' lis ((namespace, xs):levels) -- Branch 2


-- | Is essentially the core of buildFormUrlEndings; creates all url search queries
singlefOp :: Text -> [(Namespace, [Option])] -> [Text]
-- Final Cases:
singlefOp str [] = []
singlefOp str (("", _):_) = undefined
singlefOp str ((namespace, []):[]) = [] -- allows for => x : singlefOp
singlefOp str ((namespace, []):levels) = singlefOp str levels --omitting potential path of namespace=""
singlefOp str ((namespace, x:[]):levels) = singlefOp (toStr2 str namespace x) levels
singlefOp str ((namespace, xs):[]) = [ toStr2 str namespace x | x <- xs ]
-- Main: 
singlefOp str ((namespace, (x:xs)):levels) = -- Divide ->
  singlefOp (toStr2 str namespace x) levels -- Branch 1 
  <> singlefOp str ((namespace, xs):levels) -- Branch 2




  
-- singlefOp s xs = [pack $ show (take 5 xs)] 



-- singlefOp :: Text -> [(Namespace, [Option])] -> [Text]
-- singlefOp str [] = [] 
-- singlefOp str ((namespace, []):[]) = [] -- allows for => x : singlefOp
-- singlefOp str ((namespace, xs):[]) = [ toStr2 str namespace x | x <- xs ]
-- singlefOp str ((namespace, x:[]):levels) = singlefOp (toStr2 str namespace x) levels
-- singlefOp str ((namespace, (x:xs)):levels) =
--   singlefOp (toStr2 str namespace x) levels <> singlefOp str ((namespace, xs):levels)
-- singlefOp 
--                                       --still same namespace but next iteration of namespace
--                                       --with new value
-- -- | This ^ creates actual strings then recurses with (:)
-- | Creates specifically last branch ^^
-- levels to go (below) no levels to go (up)

-- | So maybe f takes a state parameter that is determined by the higher-level-set value


-- Could also be that we'll need slightly different unpacking for going across vs in
  --and that one could end up as an mconcat / <>

-- both in || to side deal with leftover params
  

-- | SO add the results of f :: ItemOfCurrentList -> [ItemOCL] -> [String]
  
-- | for basic
createBasicQKVPair :: Elem' a -> (Namespace, Option) 
createBasicQKVPair elem =
  (  (pack (fromJust (Map.lookup "name" (attrs elem)))) , (pack (findWithDefault "" "value" (attrs elem))) )
                          -- <> "=" <> 
                          

createBasicQKVPairs :: [Elem' a] -> QueryString -- [(Namespace, Option)] 
createBasicQKVPairs [] = []
createBasicQKVPairs (elem:elems) = createBasicQKVPair elem : createBasicQKVPairs elems

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

-- radiosToMap :: [Elem' a] -> Map Namespace [Option]
-- radiosToMap (e:es) = singleton _name _value <> radiosToMap
  -- where
    -- _name <+> _value >>= Namespace Option


type NamespacePair = (Namespace, Text)

mkNameVal :: Elem' a -> NamespacePair
mkNameVal = (\a -> (pack . fromJust $ Map.lookup "name" a, pack . fromJust $ Map.lookup "value" a)) . attrs

radiosToMap :: [NamespacePair] -> Map Namespace [Option]
radiosToMap [] = mempty
radiosToMap ((name, value):nps) = insertWith (<>) name (value:[]) (radiosToMap nps)



-- let
--   func new old = new : old

  
  
-- do :: Maybe a 
--   case Map.lookup "name" a of
--     Just name -> adjust name (value:) (radiosToMap es)
--     Nothing -> singleton name value <> radiosToMap
  

--     . (\a -> (Map.lookup "name" a, Map.lookup "value")) . attrs

--     -- goal is to add name if name is new and

    
                           -- Map Attr Value
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

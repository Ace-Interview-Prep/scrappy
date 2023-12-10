{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}


{-|
Description: In a way, this is the most central component of the entire library;

DOM -> Link >>= request --> DOM -> Link ...
    ^^ 
    this may be infinitely complicated by stuff such as JS 

The recursive nature of scraping is the central data structure of a URL 

Which makes me think that there may be more to consider at some point with the modern-uri package
And doing stuff such as building site trees 


-}

module Scrappy.Links where


-- import Scrappy.Elem.Types (Elem'(..), ElemHead, innerText')
-- import Scrappy.Elem.ElemHeadParse (hrefParser)
-- import Find (findSomeHTMLNaive)

-- import qualified Network.URI as URI 

import Control.Monad (join)
import qualified Network.URI as NURI
-- TODO(galen): Replace with Network.URI and deprecate Text.URI
import Text.URI (URI, uriQuery, mkURI, uriPath, unRText, emptyURI, uriScheme, uriAuthority, RTextLabel(..))
import Control.Lens ((^.))
import qualified Text.URI.Lens as UL
import Text.Parsec (ParsecT, Stream )
import Data.Functor.Classes (eq1)
import Data.Map (Map)
import Data.Either (fromRight, isRight)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.List (isSuffixOf, isInfixOf, isPrefixOf)
import qualified Data.List.NonEmpty as NE (length, last)
import Data.Text (Text, pack, unpack)
import Data.Char (toLower)

import Data.Aeson.TH (defaultOptions, deriveJSON)

type PageNumber = Int

-- |
type BaseUrl = Link
type Url = String 



type HrefURI = String 

-- TODO(galen): make this a Link 
type CurrentUrl = Url  

type DOI = String -- Change to URI if this works 

-- linkToURI :: Link -> URI
-- linkToURI = undefined

-- evalLink :: Link -> String
-- evalLink = linkToText
--   where
--     linkToText x = case x of
--       OuterPage x' -> x'
--       SearchFormURL y -> y
--       ListingPage _ _ _ _ -> undefined
--       PageHasPdf r -> r
--       Sourcery _ _ -> undefined



type Src = Url
type RelativeUrl = Url 


fixRelativeUrl :: BaseUrl -> Url -> Url
fixRelativeUrl (Link bUrl) url
  | url == "" = bUrl 
  | url == "/" = bUrl 
  | isInfixOf bUrl url = url
  | last bUrl == '/' && (isPrefixOf "/" url) = bUrl <> (tail url) -- both
  | last bUrl == '/' && (not $ isPrefixOf "/" url) = bUrl <> url  -- a 
  | last bUrl /= '/' && (isPrefixOf "/" url) = bUrl <> url -- b 
  | last bUrl /= '/' && (not $ isPrefixOf "/" url) = bUrl <> "/" <> url -- neither 

   --- || ((last bUrl /= '/') && (isPrefixOf "/" url)) = bUrl <> url

-- fixRelativeURI :: UURI -> URI.URI -> URI.URI
-- fixRelativeURI base relative = undefined
--   -- confirm that it truly is relative
  -- 

-- | Could set last url in state 
getHtmlStateful :: Url -> {- StateT SiteDetails -} String
getHtmlStateful = undefined

-- Whatever man
type LastUrl = Link
type Href = String 


fixSameSiteURL :: LastUrl -> Href -> Maybe Url
fixSameSiteURL lastUrl href = undefined


-- | Generic algorithm for determining full path given last url 
fixURL :: LastUrl -> Href -> Url
fixURL previous href = 
  -- checkIfSchemeInHref
  let
    base = if isPrefixOf "/" href then fromJust $ deriveBaseUrl previous else previous
    hrefURI = mkURI . pack $ href
  in
    case join $ uriScheme <$> hrefURI of
      -- We could easily check here if authority is the same 
      Just _ -> href 
      Nothing -> fixRelativeUrl base href 
        -- checkIfRelativeToLast -- doesnt start with /
        -- case isPrefixOf "/" href of
        --   True -> fixRelativeUrl (deriveBaseUrl previous) href
        --   False -> fixRelativeUrl previous href 
 
-- fixURL :: LastUrl -> Href -> Maybe Url
-- fixURL prev href = do

-- -- | The fromJust should never be called if Links are used properly
-- deriveBaseUrl :: Link -> BaseUrl
-- deriveBaseUrl (Link url) = Link $ mkBaseUrl $ fromJust $ mkURI . pack $ url 

-- -- | I think this is good (might also bee good lens practice tho to simplify)
-- mkBaseUrl :: URI -> String
-- mkBaseUrl uri =
--   (unpack $ ((unRText . fromJust) $  uri ^. UL.uriScheme))
--   <> ("://")
--   <> (unpack (unRText $ (fromRight undefined (uri ^. UL.uriAuthority)) ^. UL.authHost))
  
------------------------------

-- | the fromJust should never be called if Links are used properly
deriveBaseUrl :: Link -> Maybe BaseUrl
deriveBaseUrl (Link url) = mkBaseUrl =<< (mkURI . pack $ url)


-- | I think this is good (might also bee good lens practice tho to simplify)
mkBaseUrl :: URI -> Maybe Link 
mkBaseUrl uri = do
  scheme <- fmap unRText $ uri ^. UL.uriScheme
  host <- case uri ^. UL.uriAuthority of
    Right author -> Just $ unRText $ author ^. UL.authHost 
    Left _ -> Nothing  
  Just . Link $ (unpack scheme) <> ("://") <> (unpack host)
  
  -- (unpack $ ((unRText . fromJust) $  uri ^. UL.uriScheme))
  -- <> ("://")
  -- <> (unpack (unRText $ (fromRight undefined (uri ^. UL.uriAuthority)) ^. UL.authHost))


                    
  -- if yes 
  --   then weNeedTheLastUrl
  --   else deriveBaseUrl 
  -- deriveBaseUrl

-- for the scraper, instead of keeping the baseURL we should store the current URL
-- which can still be used to derive the

-- baseURL :: SiteDetails (-> CurrentUrl ->) -> BaseUrl 
                               
class IsLink a where
  renderLink :: a -> Url 




-- lets view Link as meant to contain Informationally derived meaning from internet ; that contains how to get
-- Keeps state for generic streaming 
-- data Link = OuterPage String
--           | SearchFormURL  String
--           | ListingPage [GeneratedLink] PageNumber PageKey String
--           | PageHasPdf String
--           --  PdfLink String
--           | Sourcery (PdfLink) ReferenceSys

-- pageKey=param




getFileName :: Link -> Maybe String
getFileName = getLastPath




doiParser :: ParsecT s u m DOI 
doiParser = undefined
  -- baseURL is doi.org
  
  -- isDOI :: Url -> Bool 

data ReferenceSys = RefSys [String] [String]

type GeneratedLink = String


-- type PdfLink = String   
-- | Name and Namespace are really same shit; might just converge
-- | Refer to literally "name" attribute
type Namespace = Text

-- | This is an operationally focused type where
-- | a certain namespace is found to have n num of Options
type Option = Text 


-- | More for show / reasoning rn .. non-optimal
data QParams = Opt (Map Namespace [Option]) | SimpleKV (Text, Text)



-- SiteTree can be modelled as a stream ; just depends on how we apply it -- if lazily
-- | Inter site urls and whether they have been checked for some pattern
type SiteTree = [(Bool, Text)]


-- | This wouldnt need to be exported as our interfaces would implement it under the hood
-- | and return a Link'
data DOMLink = Href' Href
             | Src Url
             | PlainLink Url 
--               | LastUrl' String
          

-- scrapeSameSiteLinks :: ParsecT s u m Link
-- scrapeSameSiteLinks = undefined

-- scrapeLinks :: ParsecT s u m Link
-- scrapeLinks = undefined


newtype Link = Link Url deriving (Eq, Show, Read, Ord)

-- | This is a general interface for extracting a raw link
-- | from scraping according to specs about the scraper itself
-- | IE if it is 100% same site
-- parseLink :: Bool -> Link -> Url -> Maybe Link
-- parseLink False lastUrl someLink =
--   --- Can be any Authority 
--   if elem (fromMaybe "" (fmap NURI.uriScheme $ NURI.parseURI someLink)) ["https:", "http:"]
--   then 
--     if (join $ fmap uriScheme $ mkURI . pack $ someLink) /= Nothing
--     then Just . Link $ someLink
--     else Just . Link $ fixRelativeUrl (fromJust $ deriveBaseUrl lastUrl) someLink
--   else
--     Nothing 
-- parseLink True lastUrl someLink 
--   | sameAuthority someLink lastUrl
--     && (elem (fromMaybe "" (fmap NURI.uriScheme $ NURI.parseURI someLink)) ["https:", "http:"])
--   = Just . Link $ fixURL lastUrl someLink

--   | otherwise = Nothing


-- | TODO(galen): fix this broken bullshitachen with Text.URI vs Network.URI
-- | generally Network.URI is better but it doesnt work with relative URLs

parseLink :: Bool -> Link -> Url -> Maybe Link
parseLink sameSite lastLink newLink = 
  case (join $ fmap uriScheme $ mkURI . pack $ newLink) == Nothing of
    True ->
      -- must be same site
      -- it doesnt matter the preference/restriction
      Just . Link $ fixRelativeUrl (fromJust $ deriveBaseUrl lastLink) newLink
    False ->
      case elem (fromMaybe "" (fmap NURI.uriScheme $ NURI.parseURI newLink)) ["https:", "http:"] of
        True ->
          let
            f sSite lastL newL
              | sSite && sameAuthority newL lastL = Just . Link $ fixURL lastL newL -- overkill but idc 
              | sSite && (not $ sameAuthority newL lastL) = Nothing 
              | not sSite = Just . Link $ newL -- must be full url
              | otherwise = undefined -- can this happen?
          in
            f sameSite lastLink newLink 

          -- -- if must be same site and is then Just Link
          -- -- for no pref
          -- if sameSite && sameAuthority newLink lastLink
          -- then Just . Link $ fixURL lastUrl someLink
          -- else
          --   if sameSite && (not $ sameAuthority newLink lastLink)
          --   then Nothing
          --   else 
          
        False ->
          -- always nothing 
          Nothing 
           
      
      -- might not be same site
      -- must be of [http, https]
    

-- -- | This assumes two full paths aka Link's 
-- diffAuthority :: Url -> Link -> Bool 
-- diffAuthority a (Link b) =
--   let
--     authA = maybe (Left False) uriAuthority (mkURI $ pack a)
--     authB = maybe (Left False) uriAuthority (mkURI $ pack b)
--   in (isRight authA) && (isRight authB) && (authA /= authB)   

sameAuthority :: Url -> Link -> Bool
sameAuthority href (Link linky) =
  let
    authA = fmap (NURI.uriRegName) $ NURI.uriAuthority =<< NURI.parseURI href
    authB = fmap (NURI.uriRegName) $ NURI.uriAuthority =<< NURI.parseURI linky
  in
    case (==) <$> authA <*> authB of
      Just True -> True -- cuz asked if diff
      _ -> False
  
-- scrapeSameSiteLinks :: CurrentUrl -> 





type HostName = String 
-- MOVE TO SCRAPPY
getHostName :: Link -> Maybe HostName
getHostName (Link url) = do 
  -- let
  --   -- this really shouldnt happen and if it is truly invalid, an error
  --   -- will happen from getHtml'
  --   uri = fromRight emptyURI $ mkURI $ pack url

    
    
  -- in fmap (unpack . unRText . (^. UL.authHost)) (uri ^. UL.uriAuthority)

  -- case mkURI $ pack url of 
  --   Just a -> ""
  --   Nothing 

  uri <- mkURI $ pack url
  case fmap (unpack . unRText . (^. UL.authHost)) (uri ^. UL.uriAuthority) of
    Right hn -> Just hn
    _ -> Nothing 
    

-- getHostName :: Link -> Either Bool HostName
-- getHostName 

  
-- | Only exported interface 
instance IsLink Link where
  renderLink (Link url) = url 



-- newtype SameSiteT m a = SameSiteT { runSST :: StateT LastUrl m a } 

--getHtmlST :: sv -> Link -> m (sv, Html) 


-- -- | In reality, this is 4 helper functions 
-- link :: (Maybe LastUrl) -> ScraperT Link
-- link onlyThisSite = do
--   link' <- parseOpeningTag linkStuff
--   validateLink onlyThisSite link' 


-- doesnt have a scheme:
--   NoScheme -> must be same site and relative;-> Just $ relative to current or base URL ? 
--   HasScheme -> if mustBeSS && isSSite then Just url else Nothing 


-- -- validateLink is gonna be an interface that may use fixURL and sees if its the same site 
-- -- | All 4 scrapers would use validateLink 
-- validateLink :: Bool -> LastUrl -> DOMLink -> Link
-- validateLink ots lastUrl iLink = case ots of
--   True -> ""
--   False -> "" 


-- Note following ideas

-- data Source = Source (Citations, Html)


type Html = String -- or could be just the pdf, but maybe even URL for storage sake --> could become research graph
                   -- but in a sense, would be a forest of uncited (yet) publicationsop

-- findAdvancedSearchLinks :: ParsecT s u m [String]
-- findAdvancedSearchLinks = undefined


-- | Core function of module, filters for any links which point to other pages on the current site
-- | and have not been found over the course of scraping the site yet 
-- | filters out urls like https://othersite.com and "#"
maybeUsefulNewUrl :: Link -> [(Link, a)] -> Link -> Maybe Link
maybeUsefulNewUrl baseUrl tree url = maybeUsefulUrl baseUrl url >>= maybeNewUrl tree 




urlIsNew :: [(a, Url)] -> HrefURI -> Bool
urlIsNew [] uri = True
urlIsNew (branch:tree) uri
  | eq1 (fmap uriPath (mkURI' (uri))) (fmap uriPath (mkURI' (snd branch))) = False
  | otherwise = urlIsNew tree uri
  where
    mkURI' :: String -> Maybe URI
    mkURI' url = mkURI (pack url)



maybeNewUrl :: [(Link, a)] -> Link -> Maybe Link
maybeNewUrl [] uri = Just uri
maybeNewUrl (branch:tree) uri =
  if eq1 (fmap uriPath (mkURI' (renderLink uri))) (fmap uriPath (mkURI' . renderLink . fst $ branch))
  then Nothing
  else maybeNewUrl tree uri
  -- eq1 (fmap uriPath (mkURI' (pack uri))) (fmap uriPath (mkURI' (fst branch))) = False
  -- otherwise = urlIsNew tree uri
  where
    mkURI' :: String -> Maybe URI
    mkURI' url = mkURI (pack url)
  



-- | Filters javascript refs, inner page DOM refs, urls with query strings and those that
-- | do not contain the base url of the host site
maybeUsefulUrl :: Link -> Link -> Maybe Link
maybeUsefulUrl (Link baseUrl) url = do
  noJSorShit url
  numberOfQueryParamsIsZero url
  if isInfixOf baseUrl (renderLink url) then return url else Nothing
  allowableEndings url

  where
    noJSorShit :: Link -> Maybe Link
    noJSorShit link =
      if (not $ elem True (urlContains link ["javascript", "about", "help", "#"]))
      then Just url
      else Nothing

    urlContains :: Link -> [String] -> [Bool]
    urlContains (Link url) icases = fmap ((flip isInfixOf) (fmap toLower url)) icases
  
    allowableEndings url =
      let lastPath = fromMaybe "" $ getLastPath url
      in
        if (elem '.' lastPath)
        then allowableFile lastPath url -- must be of allowable
        else Just url
  
    allowableFile endPath url =

      if elem True $ fmap (\x -> isSuffixOf x (fmap toLower endPath)) allowed
      then Just url
      else Nothing
      where allowed = [".aspx", ".html", ".pdf", ".php"]

    
-- getLastPath :: Url -> String
-- getLastPath url = unpack (unRText (NE.last (snd (fromJust (fromJust (fmap uriPath (mkURI (pack url))))))))

getLastPath :: Link -> Maybe String
getLastPath (Link url) = do 
  x <- mkURI $ pack url 
  x' <- uriPath x
  Just . unpack . unRText . NE.last . snd $ x'

-- | Input is meant to be right from 
usefulNewUrls :: Link -> [(Link, a)] -> [Link] -> [Maybe Link]
usefulNewUrls _ _ [] = []
usefulNewUrls baseUrl tree (link:links) = (maybeUsefulNewUrl baseUrl tree link) : usefulNewUrls baseUrl tree links

usefulUrls :: Link -> [Link] -> [Maybe Link]
usefulUrls baseUrl (link:links) = maybeUsefulUrl baseUrl link : usefulUrls baseUrl links 

numberOfQueryParamsIsZero :: Link -> Maybe String
numberOfQueryParamsIsZero (Link uri) = do
  x <- mkURI (pack uri)
  if length (uriQuery x) == 0
  then Just uri
  else Nothing


deriveJSON defaultOptions ''Link

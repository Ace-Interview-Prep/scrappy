{-# LANGUAGE TupleSections #-} 

module Scrappy.Links where


import Scrappy.Elem.Types (Elem'(..), ElemHead, innerText', getHref)
import Scrappy.Elem.ElemHeadParse (hrefParser)
-- import Find (findSomeHTMLNaive)


import Text.URI (URI, uriQuery, mkURI, uriPath, unRText, emptyURI )
import Control.Lens ((^.))
import qualified Text.URI.Lens as UL
import Text.Parsec (ParsecT)
import Data.Functor.Classes (eq1)
import Data.Map (Map)
import Data.Either (fromRight)
import Data.Maybe (catMaybes, fromJust)
import Data.List.Extra (isSuffixOf, isInfixOf)
import qualified Data.List.NonEmpty as NE (length, last)
import Data.Text (Text, pack, unpack)
import Data.Char (toLower)



type PageNumber = Int

type BaseUrl = Url 
type Url = String 



type HrefURI = String 




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
      

data Clickable = Clickable ElemHead Url deriving (Eq, Show)

-- lets view Link as meant to contain Informationally derived meaning from internet ; that contains how to get
-- Keeps state for generic streaming 
-- data Link = OuterPage String
--           | SearchFormURL  String
--           | ListingPage [GeneratedLink] PageNumber PageKey String
--           | PageHasPdf String
--           --  PdfLink String
--           | Sourcery (PdfLink) ReferenceSys

-- pageKey=param


-- | In the future this definitely could be expanded upon for our JS interface
-- | right now this only works for links but wouldn't literally click a button 
mkClickable :: ElemHead -> Elem' a -> Maybe Clickable
mkClickable eHead emnt = do
  href <- getHref emnt
  pure $ Clickable eHead href



getFileName :: Url -> String
getFileName = getLastPath


-- | I think this is good (might also bee good lens practice tho to simplify)
-- | Move to scrappy?
mkBaseUrl :: URI -> String
mkBaseUrl uri =
  (unpack $ ((unRText . fromJust) $  uri ^. UL.uriScheme))
  <> ("://")
  <> (unpack (unRText $ (fromRight undefined (uri ^. UL.uriAuthority)) ^. UL.authHost))



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



-- Note following ideas

-- data Source = Source (Citations, Html)


type Html = String -- or could be just the pdf, but maybe even URL for storage sake --> could become research graph
                   -- but in a sense, would be a forest of uncited (yet) publicationsop

findAdvancedSearchLinks :: ParsecT s u m [String]
findAdvancedSearchLinks = undefined


-- | Core function of module, filters for any links which point to other pages on the current site
-- | and have not been found over the course of scraping the site yet 
-- | filters out urls like https://othersite.com and "#"
maybeUsefulNewUrl :: String -> [(Url, a)] -> HrefURI -> Maybe HrefURI
maybeUsefulNewUrl baseUrl tree url = maybeUsefulUrl baseUrl url >>= maybeNewUrl tree 




urlIsNew :: [(a, Url)] -> HrefURI -> Bool
urlIsNew [] uri = True
urlIsNew (branch:tree) uri
  | eq1 (fmap uriPath (mkURI' (uri))) (fmap uriPath (mkURI' (snd branch))) = False
  | otherwise = urlIsNew tree uri
  where
    mkURI' :: String -> Maybe URI
    mkURI' url = mkURI (pack url)



maybeNewUrl :: [(Url, a)] -> HrefURI -> Maybe HrefURI
maybeNewUrl [] uri = Just uri
maybeNewUrl (branch:tree) uri =
  if eq1 (fmap uriPath (mkURI' (uri))) (fmap uriPath (mkURI' (fst branch)))
  then Nothing
  else maybeNewUrl tree uri
  -- eq1 (fmap uriPath (mkURI' (pack uri))) (fmap uriPath (mkURI' (fst branch))) = False
  -- otherwise = urlIsNew tree uri
  where
    mkURI' :: String -> Maybe URI
    mkURI' url = mkURI (pack url)
  



-- | Filters javascript refs, inner page DOM refs, urls with query strings and those that
-- | do not contain the base url of the host site
maybeUsefulUrl :: String -> HrefURI -> Maybe HrefURI
maybeUsefulUrl baseUrl url = do
  noJSorShit url
  numberOfQueryParamsIsZero url
  if isInfixOf baseUrl url then return url else Nothing
  allowableEndings url

  where
    noJSorShit :: String -> Maybe String
    noJSorShit url =
      if (not $ elem True (urlContains url ["javascript", "about", "help", "#"]))
      then Just url
      else Nothing

    urlContains :: String -> [String] -> [Bool]
    urlContains url icases = fmap ((flip isInfixOf) (fmap toLower url)) icases
  
    allowableEndings url =
      let lastPath = getLastPath url
      in
        if (elem '.' lastPath)
        then allowableFile lastPath url -- must be of allowable
        else Just url
  
    allowableFile endPath url =

      if elem True $ fmap (\x -> isSuffixOf x (fmap toLower endPath)) allowed
      then Just url
      else Nothing
      where allowed = [".aspx", ".html", ".pdf", ".php"]

    
getLastPath :: Url -> String
getLastPath url = unpack (unRText (NE.last (snd (fromJust (fromJust (fmap uriPath (mkURI (pack url))))))))


newUrlList :: [Maybe Text] -> [(Bool, Text)] -> [(Bool, Text)]
newUrlList newUrls oldUrls = fmap (False,) (catMaybes newUrls) <> oldUrls

-- | Input is meant to be right from 
usefulNewUrls :: String -> [(Url, a)] -> [Url] -> [Maybe HrefURI]
usefulNewUrls _ _ [] = []
usefulNewUrls baseUrl tree (link:links) = (maybeUsefulNewUrl baseUrl tree link) : usefulNewUrls baseUrl tree links

usefulUrls :: String -> [Url] -> [Maybe HrefURI]
usefulUrls baseUrl (link:links) = maybeUsefulUrl baseUrl link : usefulUrls baseUrl links 

numberOfQueryParamsIsZero :: String -> Maybe String
numberOfQueryParamsIsZero uri = do
  x <- mkURI (pack uri)
  if length (uriQuery x) == 0
  then Just uri
  else Nothing

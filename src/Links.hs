{-# LANGUAGE TupleSections #-} 

module Links where


import Elem.Types (Elem', innerText')
import Elem.ElemHeadParse (hrefParser)
-- import Find (findSomeHTMLNaive)

import qualified Data.List.NonEmpty as NE (length, last)
import Data.Text (Text, pack, unpack)
import Text.URI (URI, uriQuery, mkURI, uriPath, unRText )
import Text.Parsec (ParsecT)
import Data.Maybe (catMaybes, fromJust)
import Data.Char (toLower)
import Data.List.Extra (isSuffixOf, isInfixOf)
import Data.Functor.Classes (eq1)
import Data.Map (Map)


type PageNumber = Int

type Url = Text



type HrefURI = String 




type DOI = String -- Change to URI if this works 

linkToURI :: Link -> URI
linkToURI = undefined

evalLink :: Link -> String
evalLink = linkToText
  where
    linkToText x = case x of
      OuterPage x' -> x'
      SearchFormURL y -> y
      ListingPage _ _ _ _ -> undefined
      PageHasPdf r -> r
      Sourcery _ _ -> undefined
      


-- lets view Link as meant to contain Informationally derived meaning from internet ; that contains how to get
-- Keeps state for generic streaming 
data Link = OuterPage String
          | SearchFormURL  String
          | ListingPage [GeneratedLink] PageNumber PageKey String
          | PageHasPdf String
          | PdfLink String
          | Sourcery (PdfLink) ReferenceSys

-- pageKey=param
type PageKey = String

data ReferenceSys = RefSys [String] [String]

type GeneratedLink = String


type PdfLink = String   
-- | Name and Namespace are really same shit; might just converge
-- | Refer to literally "name" attribute
type Namespace = Text

-- | This is an operationally focused type where
-- | a certain namespace is found to have n num of Options
type Option = Text 


-- | More for show / reasoning rn .. non-optimal
data QParams = Opt (Map Namespace [Option]) | SimpleKV (Text, Text)



-- SiteTree can be modelled as a stream ; just depends on how we apply it -- if lazily
type SiteTree = [(Text, Bool)]



-- Note following ideas

-- data Source = Source (Citations, Html)


type Html = String -- or could be just the pdf, but maybe even URL for storage sake --> could become research graph
                   -- but in a sense, would be a forest of uncited (yet) publicationsop

findAdvancedSearchLinks :: ParsecT s u m [String]
findAdvancedSearchLinks = undefined






urlIsNew :: SiteTree -> HrefURI -> Bool
urlIsNew [] uri = True
urlIsNew (branch:tree) uri
  | eq1 (fmap uriPath (mkURI' (pack uri))) (fmap uriPath (mkURI' (fst branch))) = False
  | otherwise = urlIsNew tree uri
  where
    mkURI' :: Text -> Maybe URI
    mkURI' url = mkURI url



maybeNewUrl :: SiteTree -> HrefURI -> Maybe HrefURI
maybeNewUrl [] uri = Just uri
maybeNewUrl (branch:tree) uri =
  if eq1 (fmap uriPath (mkURI' (pack uri))) (fmap uriPath (mkURI' (fst branch)))
  then Nothing
  else maybeNewUrl tree uri
  -- eq1 (fmap uriPath (mkURI' (pack uri))) (fmap uriPath (mkURI' (fst branch))) = False
  -- otherwise = urlIsNew tree uri
  where
    mkURI' :: Text -> Maybe URI
    mkURI' url = mkURI url
  
 
maybeUsefulNewUrl :: String -> HrefURI -> SiteTree -> Maybe HrefURI
maybeUsefulNewUrl baseUrl url tree = maybeUsefulUrl baseUrl url >>= maybeNewUrl tree 



-- Useful as in, diff web page on same site 
maybeUsefulUrl :: String -> HrefURI -> Maybe HrefURI
maybeUsefulUrl baseUrl url = do
  url2 <- noJSorShit url
  
  url3 <- numberOfQueryParamsIsZero url2
  url4 <- if isInfixOf baseUrl url3 then return url3 else Nothing
  allowableEndings url4 
  where
    noJSorShit :: String -> Maybe String
    noJSorShit url =
      if (elem True (urlContains url ["javascript", "about", "help", "#"]))
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

    getLastPath url = unpack (unRText (NE.last (snd (fromJust (fromJust (fmap uriPath (mkURI (pack url))))))))




newUrlList :: [Maybe Text] -> [(Text, Bool)] -> [(Text, Bool)]
newUrlList newUrls oldUrls = fmap (, False) (catMaybes newUrls) <> oldUrls

-- | Input is meant to be right from 
usefulNewUrls :: String -> SiteTree -> [Text] -> [Maybe HrefURI]
usefulNewUrls _ _ [] = []
usefulNewUrls baseUrl tree (link:links) = (maybeUsefulNewUrl baseUrl (unpack link) tree) : usefulNewUrls baseUrl tree links

numberOfQueryParamsIsZero :: String -> Maybe String
numberOfQueryParamsIsZero uri = do
  x <- mkURI (pack uri)
  if length (uriQuery x) == 0
  then Just uri
  else Nothing

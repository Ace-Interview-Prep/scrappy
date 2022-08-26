# Scrappy #


provides a number of functions that allow for easily scraping certain patterns from websites as well control functions that allow you to rotate between multiple sites and rotate proxies in order to deal with common bot detection problems faced by scrapers. 

## Scraping ## 

Scraping is parsing, where we don't care about the placement of our match in a chunk of data. 

## Main functions ## 

```haskell
-- | Manager returned may be a new manager, if the given manager failed 
getHtml :: Manager -> Url -> IO (Manager, Html) 
getHtml' :: Url -> IO Html

runScraperOnHtml :: ParsecT s u Identity a -> Html -> Maybe [a]

-- | For more advanced control over inner structure see Elem.TreeElemParser
elemParser :: Elem -> [(String, Maybe String)] 
            -> Maybe (ParsecT s u m a) -- Optionally scraped pattern inside this el, if specified, return element must have at least 1 
            -> ParsecT s u m (Elem' a)

el :: Elem -> [(String, String)] -> ParsecT s u m (Elem' String)

-- | Find any expressive pattern as long as it is inside of some HTML context 
contains :: ParsecT s u m (Elem' a) -> ParsecT s u m a -> ParsecT s u m a 


:t fmap snd getHtml' "https://google.com" >>= return . (runScraperOnHtml (el "a" [])   
>>> IO (Maybe [Elem' a])

-- Will get all <a id="link"> 's on the url that are inside
-- divs with a class of xyz
example = runScraperOnUrl url $ el "div" [("class", "xyz")] `contains`
        el "a" [("id", "link")]


```

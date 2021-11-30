# Scrappy #

provides a number of functions that allow for easily scraping certain patterns from websites as well control functions that allow you to rotate between multiple sites and rotate proxies in order to deal with common bot detection problems faced by scrapers. 

Main functions:

```haskell
getHtml :: Manager -> Url -> IO (Manager, Html) 


elemParser :: Elem -> [(String, Maybe String)] 
            -> Maybe (ParsecT s u m a) -- Optionally scraped pattern inside this el, if specified, return element must have at least 1 
            -> ParsecT s u m (Elem' a)

el :: Elem -> [(String, String)] -> ParsecT s u m (Elem' String)

runScraperOnHtml :: ParsecT s u Identity a -> Html -> Maybe [a]

-- | Find any expressive pattern as long as it is inside of some HTML context 
contains :: ParsecT s u m (Elem' a) -> ParsecT s u m a -> ParsecT s u m a 


:t fmap snd getHtml' "https://google.com" >>= return . (runScraperOnHtml (el "a" [])   
>>> IO (Maybe [Elem' a]) 
```

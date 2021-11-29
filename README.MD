# Scrappy #

provides a number of functions that allow for easily scraping certain patterns from websites as well control functions that allow you to rotate between multiple sites and rotate proxies in order to deal with common bot detection problems faced by scrapers. 

Main functions:

```code
getHtml :: Manager -> Url -> IO (Manager, Html) 

elemParser :: Elem -> [(String, Maybe String)] -> Maybe (ParsecT s u m a) -> ParsecT s u m (Elem' a)

runScraperOnHtml :: ParsecT s u Identity a -> Html -> Maybe [a]
```

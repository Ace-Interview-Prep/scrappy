module Scrappy.Config where

data Browser sv = Browser { currentUrl :: Url
                          , history :: Map BaseUrl [Url]
                          , seshVar :: sv
                          , javascriptLogger :: (String -> IO ())
                          , headers :: (Headers -> Headers)
                          , authAndCookies :: CookieJar 
                          }

type Headers = [String]
                  
-- data SingleSite sv = SingleSite { 

newtype BrowserT sv m a = BrowserT { runBrowserT :: StateT (Browser sv) m a }

-- -- | I Could also replace this with just simply a BrowserT monadic action 
-- newtype SameSiteT sv m a = SameSiteT { runSameSiteT :: StateT (Url, [Url], sv) m a 




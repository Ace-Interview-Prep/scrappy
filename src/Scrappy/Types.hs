{-# LANGUAGE KindSignatures #-}

module Scrappy.Types where

-- may change Types + Links -> Navigation + Something else 



import Network.HTTP.Client (CookieJar, requestBody, method, destroyCookieJar, responseCookieJar, httpLbs, RequestBody (RequestBodyBS, RequestBodyLBS), parseRequest, newManager, responseBody, Manager)
import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Lazy (StateT)
import Data.Time.Clock.System
import Data.Text (Text)



data CookieManager = CookieManager CookieJar Manager 
    


data ScrapeFail = Eof | NonMatch

-- | Note: both elemParser and treeElemParser are capable of doing greedy or non-greedy matching
  --treeElemParser (unless its really slow) should be better for non-greedy/focused
  --elemParser should be better for greedy






  

-- IO for requests
-- Either for high level important errors
-- Maybe for Naive scraping logic 


-- State looking important for managing status of each site as well
-- as scrape coin 

-- |  data Processor a b = Processor ThreadId { runFunc :: (a -> b) }

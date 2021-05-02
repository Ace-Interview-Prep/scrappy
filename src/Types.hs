{-# LANGUAGE KindSignatures #-}

module Types where

-- may change Types + Links -> Navigation + Something else 



import Network.HTTP.Client (CookieJar, requestBody, method, destroyCookieJar, responseCookieJar, httpLbs, RequestBody (RequestBodyBS, RequestBodyLBS), parseRequest, newManager, responseBody, Manager)
import Control.Concurrent (ThreadId)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Lazy (StateT)
import Data.Time.Clock.System
import Data.Text (Text)


-- Where stream represents a complex homogenous type
class DataStream (a :: * -> *) where
  concurrentStream :: (b -> b) -> a b -> a b  
  headS :: a b -> b
  tail :: a b -> a b
  singleton :: b -> a b
  --any other list-like functions


instance DataStream [] where
  concurrentStream f stream = stream <> (f (head stream):[])


type SiteState = () 
    
data GlobalState a = GlobalState Manager [SiteState]
-- where Stream a => GlobalState a in use 

type DefaultGlobalState = GlobalState Text




type CategoryID = Int
type Subject = String 


-- | Note: both elemParser and treeElemParser are capable of doing greedy or non-greedy matching
  --treeElemParser (unless its really slow) should be better for non-greedy/focused
  --elemParser should be better for greedy


data ScrapeException = NoLinksOnPage
                     | NoAdvSearchJustBasic
                     | NoSearchOrBrowseOnSite
                     | CouldntFindResultsWithSearchOrBrowse
                     | CouldntPaginate
                     | PaginationMaybeComplete
                     | ItemHasNoFindablePdf
                     | PDFRequestFailed
                     | NoListItems String 
-- see htmlTrees.hs for newer idea

                 --might rename to EndStream
data ScrapeFail = Eof | NonMatch


-- Just to model concurrency 
-- type ScrapeCoin = Int
-- | OR
type ScrapeTimeState = SystemTime
-- then do diff t1 t2 or diff t1 t_Max_Number --> Continue if output_t > min_t
-- Could even combine both systems

-- | Need to use HistoriedResponse on request of UWO hrefs in order to get last redirect

-- type MyScraperMonad a = ExceptT ScrapeError IO a 


type Tag = String 

  
type MyMonad a = IO (Either ScrapeException (Maybe a))
-- IO for requests
-- Either for high level important errors
-- Maybe for Naive scraping logic 


-- State looking important for managing status of each site as well
-- as scrape coin 

-- |  data Processor a b = Processor ThreadId { runFunc :: (a -> b) }

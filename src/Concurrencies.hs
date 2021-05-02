module Concurrencies where

-- | Goal: Use STM to model " has it been enough time in between scrapes? "
  -- `retry` if not

-- | May need to have sub datatype for (RemainingLinksOnPage + Maybe PdfLink) 

data Site = ([Genre], SiteState)

data SiteState = BasicPages [Text]
               -- | Search SearchResults
               | AdvancedSearchPage String -- URL 
               | PerformSearch (Maybe PageKey) (Maybe CurrentQuery) [Queries] [PageItem]
               -- also could Merge (Maybe CurrentQuery ++ Queries)
                 -- just perform head then put back 
               | SiteEndState


initQuery :: String -> CurrentQuery
initQuery queryUrl = CurrentQuery 0 url
  -- case key of
  -- Just qKey -> ""
  -- Nothing -> 

-- data CurrentQuery = CurrentQuery PageNumber (Maybe PageKey) Url

-- execSiteState :: SiteState -> IO (Either ScrapeError SiteState)
-- execSiteState state = case state of
--   BasicPages knownUrls ->
--     ""
--   AdvancedSearchPage url -> do
--     body <- mkRequest url 
--     let formParser :: [Query] -- where we generate a fuck ton of query possibilities
--         formParser = undefined

--     (return . return) $ PerformSearch (initQuery Nothing (head formParser)) (tail formParser) [] 
    
--   PerformSearch maybePageKey maybeCurrentQuery queriesToDo leftoverPageItems ->
--     case leftoverPageItems of
--       (item:items) ->
--         --EDIT LOGIC, need if item is done
--         case item of
--           PdfLink url -> do
--             performDownloadPdf url 
--           TryUrl (url:urls) -> do
--             (x :: Maybe ResearchResult) <- tryToScrapeResearchResult url
--             case x of
--               Nothing -> PerformSearch maybePageKey maybeCurrentQuery queriesToDo (TryUrl urls)
--               Just -> PerformSearch maybePageKey maybeCurrentQuery queriesToDo (PdfLink url) 
--         -- (performItem item) {- :: IO () -} (:) items



        
--       [] ->
--         case maybeCurrentQuery of
--           Just (CurrentQuery num url) ->
--             let derivedKey = fromMaybe ("derivePagination") key 
--             in
--               do
--                 -- pageKey is the same as maybePageKey (using Just) if it already existed 
--                 (pageKey, items) <- return $ scrapeQueryListingPage maybePageKey query :: SiteState
--                 return $ PerformSearch (Just pageKey) (CurrentQuery (num + 1) url) queriesToDo items 
--             -- let url = 
--             -- 
--             {- _ :: 'PerformSearch -}
--           Nothing -> {- we have signaled and finished the last query -}
--             case queriesToDo of
--               [] -> (return . return) SiteEndState
--               (query:queriesToDo') ->
--                 do
--                 -- pageKey is the same as maybePageKey (using Just) if it already existed 
--                   (pageKey, items) <- return $ scrapeQueryListingPage maybePageKey initQuery (head queriesToDo) 
--                   return $ PerformSearch (Just pageKey) (CurrentQuery (num + 1) queriesToDo' items 
--                 -- scrapeQueryListingPage maybePageKey $ 
--           -- Inside of scrapeQueryListingPage is gonna be conditional derivation of page key 
--   SiteEndState ->
--     -- in theory this should never happen due to selectNext logic 
--     undefined


scrapeQueryListingPage :: Either ParsecError (PageKey, [Item])
scrapeQueryListingPage = undefined


-- mkItem :: [String] -> Item
-- mkItem urls = TryUrls urls 



-- RemainingQueries are the result of generating search options

-- do head of [TryUrl] if Nothing 
-- data PageItem = PageItem (Maybe PdfLink) [TryUrl]

-- data PageItem = PdfLink String
--                | TryUrl [String]

-- SearchResults is a very "main"-esque data structure; lots happening 
-- data SearchResults =
  -- SearchResults (Maybe CurrentQuery) [Queries] [PageItem] --(RemainingLinksOnPage) (Maybe PdfLink)
  -- get (head [PageItem]) if not []
     -- > case x of { Just pdf -> downloadPdf params; Nothing -> get,do head [TryUrl]
  -- do case currentQuery
       -- { Just q -> get (iteratePageNum)
       --   Nothing -> get,do (head [Queries] <> pgKey) 
       -- put to currentQuery (note that PageItem will == [])

-- data Link = OuterPage String
--           | SearchFormURL  String
--           | ListingPage [GeneratedLink] PageNumber PageKey String
--           | PageHasPdf String
--           | PdfLink String
--           | Sourcery (PdfLink) ReferenceSys


-- SearchResults

  -- | Bottom one would work better for not needing to do every single time
     -- Could literally append " page_key = ${Int} " at end
     -- Derivable by using current method + logic : (prefixed with parser@(char '&' <* some Letter *> char '=')
        -- could just cut off at index, reverse the string then parse with `parser` (except reversed)
        -- then just reverse the key again
     


     -- data UrlPagination = UrlPagination String Int String -- deprecated
     --  || OR ||
     -- data CurrentQuery = CurrentQuery PageNumber (Maybe PageKey) Url

  -- type RemainingQueries = [Url] -- should be lazy stream
  -- type RemainingLinksOnPage = [Url]

-- Int signifies if one link is known to be the most promising
  -- in order to limit number of requests (from eg. 5 to 1)
-- data RemainingLinksOnPage' = MkRemLinks (Maybe Int) [[String]]

-- this could also become RemainingLinksOnPage and just move that index to the front for first try then just do
-- same old try each logic

-- for iterating through RemainingLinksOnPage, should iterate not on [a] but [[a]]

-- f :: [[a]] -> [IO ()] 
-- f (x:xs) = g x -- stops iterating through x if a result was found ? 
--   where
--     g (x:xs) = 

data NewGlobalState = Proxies SafeScrape ScrapeAway

type Proxy = String 
type Proxies = [Proxy]

-- Note: this may call for table parser 

type ScrapeAway = [Site]
-- type SafeScrape = [SiteState] 

--OR
data SafeSiteScrape = (LastScraped, Site)
type SafeScrape = [SafeSiteScrape]
type LastScraped = SystemTime 

-- We need a global proxy list thats passed with state


-- will be imported and run in main, after the genres have been scraped 
-- execGlobal = 
--   if (timeOfAll3TooLittle)
--   then
--     let numOfScrapeAwayToDo = fromTimeToEstimatedNumberOftodo $ maxOfthe3 (basedOn randint)
--     in
--       execFiniteNumTimes numOfScrapeAwayToDo (ScrapeAway concurrentStream)
--   else
--     let
--       chosen = choose (the3)
--     in execState chosen


-- Note: Need proxies for ScrapeAway sites 




-- concurrent stream
-- running any scraper in parallel






-------------------------------------------------------------------------------------------------------------------

-- type Everything = [SiteState]


-- x=[SS1 x, SS2 x2, SS3 x3 .... ] -- length over 700

-- let x' = head x

-- scrapeUrlWith :: SiteState -> Url -> Whatever -> SiteState

-- f :: GlobalState 
-- f x x' = apply (x <> scrapeUrlWith (x' ...)) :: [SiteState]

-- apply x = f (head x) (tail x)

-- so peel the head, run scrapeUrlWith, then append it to end of list

-- Terminate? How?

-- scrapeUrlWith (Complete :: SiteState) = return x

-- ~~~ [1..5] <> [] ... <> [] <> [] .... <> []
-- ==> [1..5]
-- similarly: [] <> [] == []

-- so

--  x <- scrapeUrlWith i

-------------------------------------------------------------------------------------------------------------------

-- takeN x where x == NUM_PROCESSORS


-- or same but instead

-- for i in range(num_processors) -> ( let {...} AND x <- execStatef )

-- which is of type [SiteState] , named: newXs

-- globalState <> newXs

-- Only issue is when length list < num_processors
 



-- temporary fix for Processor in Types.hs
type Processor a b = (a -> b)

-- PSEUDO Code ; not actually how to do parallel processing just trying to model
applyProcessorsToState :: [Processor a b] -> IO ()
applyProcessorsToState _ [] = done
applyProcessorsToState (processor:ys) prevGlobState = do
  let
    globalState = findStartingPoint prevGlobState
    next = head globalState
      
  (tId, idx) <- mkProcessor (length ys) processor next
  applyProcessorsToState ys ((tail globalState) <> singleton (processor x))



-- applyProcessorsToState [] (x:xs) = xs 

mkProcessor :: Int -> (a -> b) -> IO (ThreadId, Int)
mkProcessor i f x = do
  tId <- forkIO f x
  return (tId, i)

-- concurrentStream :: Stream s => (a -> b) -> s a -> s b
runConcurrentStreamParallel :: DataStream s => (a -> a) -> s a -> IO (s a) 
runConcurrentStreamParallel evalStatef prevGlobState = do
  let
    globalState = findStartingPoint prevGlobState
    next = head globalState
      
  x <- evalStatef next
  if x == mempty
    then return ()
    else runConcurrentStreamParallel execStatef (tail globalState <> (singleton x))

-- | In order to run in parallel model must change

-- | foreach processFunc:
  -- | x <- take 1 from SharedMemory
  -- | y <- applyOnProcessor processFunc x 
  -- | push y to end of shared memory
    -- | mem <- getMem
    -- | return $ mem <> (y:[]) 

  -- Just need to avoid race conditions

-- | There also is guranteed to exist a function: getNumberOfCores or somethin
  -- GHC.Conc : numCapabilities ; getNumProcessors


-- | OR what if we put each SiteState to shared memory and had a list of references to each index
  -- and we then just concurrently shift the pointers/references

-- NOTE: key assumption is that we can use Manager and share it between processes


-- I believe process will be like
  -- 1) Check if available processor --- or do i create a thread? that is then run on an available processor?
  -- 2) Check if State is not locked -> then grab
    -- 2A) OR shared memory holds references in list
         -- so check if this is not locked then
            --Either (put ref at end and then Act on and store SiteState->SiteState at such address and it
            --    wont matter cuz ref exists AND file of refs is locked for minimal time but its also highly
            --    unlikely that the file ref will be acted on again before the next loop through tries to
            --    read and act on it

         -- Even if the loop returns to same ref faster than `f` processes it, the value itself will be locked
            -- for reading from any other processor with `f` that tries to act on it
            -- AND f , in its application to x, should provide recovery logic to retry after some amount of time
            -- in the event of a lock exception


-- SO ;
  -- Pre) With [SiteState] -> writeToMemoryAndGiveBackRefs :: [SiteState] -> [IORef]

  -- 1) retry until access lock file
  -- 2) duplicate ref, putting one in state2 of concurrent, and keeping other in process
  -- 3) put stream to shared memory
  -- 4) retry until we access to the given SiteState, produced by the ref/pointer
  -- ? 5) ? await available processor?
  -- 6) run `f` on SiteState on processor
  -- 7) Write to the SiteState memory location
  -- 8) Forfeit lock to SiteState memory location 



-- x <- atomically $ (do
--                       tvar <- readTvar
--                       (list, (siteState || pointer)) <- runConncurrentStream tvar
--                       writeTVar list
--                       return (siteState || pointer)
--                   )
-- let out = execState x
-- case ProcessCompleted -> out


-- atomically $ (do
--                  )


-- -- | we could apply gate logic to processors ? from paper on concurrency + STM

-- f[TVar SiteState] 

-- [SiteState] -> IO [TVar SiteState]

-- -- Concurrent implementation 

-- manager <- newManger 

-- tvar <- atomically $ (do
--                          stream <- readTVar globalState 
--                          (next, stream') <- manageConcurrentStream
--                          writeTVar globalState stream
--                          return next 
--                      )
        
-- out <- atomically $ (do
--                         siteState <- readTVar tvar
--                         guard (enoughTimeElapsed)
--                         x <- execState siteState manager
--                         writeTVar tvar x
--                     )
-- forkIO out 

-----------------------------

-- | Passes the a in STM a when it finally is gained ; as opposed to retried
-- | like a promise
-- choose :: [(STM a, a -> IO ())] -> IO ()

--helper func
manageConcurrentStream :: DataStream s => s TVar -> (s TVar, TVar)
manageConcurrentStream = undefined
  -- Duplicates TVar, (at end of stream and and in snd of tuple)

 
-- mgCStr :: s TVar -> (TVar -> IO ()) -> IO ()
-- mgCStr = undefined


-- concurrentStream :: Stream s => (a -> b) -> s a -> s b
runConcurrentStream' :: DataStream s => (a -> a) -> s a -> IO ()
runConcurrentStream' runStatef prevGlobState = do
  let
    globalState = findStartingPoint prevGlobState
    next = head globalState
      
  x <- evalStatef next
  if x == mempty
    then return ()
    else runConcurrentStream execStatef (tail globalState <> (singleton x))

-- ( a -> a ) ~~ (StateT Manager IO SiteState -> StateT Manager IO SiteState)


-- | Critical difference is that idx_N 's relevant state is passed to idx_N+1
runStatefulConcurrentStream :: DataStream s => ((b -> a) -> a) -> s a -> b -> IO ()
runStatefulConcurrentStream = undefined 
-- runStatefulConcurrentStream :: DataStream s => ((Manager -> a) -> a) -> s a -> Manager -> IO ()  

-- concurrentStream :: Stream s => (a -> b) -> s a -> s b
runConcurrentStream'' :: (MonadIO m, DataStream s) => (a -> a) -> s a -> m (s a) 
runConcurrentStream'' evalStatef prevGlobState = do
  let
    globalState = findStartingPoint prevGlobState
    next = head globalState
      
  x <- evalStatef next
  if x == mempty
    then return ()
    else runConcurrentStream execStatef (tail globalState <> (singleton x))


--------------------------------------------------------------------------------------------------------------------
------------------------------------------To DO-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- | NOTE: Concurrent stream could also easily model proxies (when processors code-section is written)



-- All recursive apply is meant to do is apply some `f` to head then
-- move the result of the end of the stream
-- concurrentStream :: Stream s => (a -> b) -> s a -> s b
runConcurrentStream :: DataStream s => (a -> a) -> s a -> IO ()
runConcurrentStream execStatef prevGlobState = do
  -- let generalizes to all stream types 
  let
    globalState = findStartingPoint prevGlobState
    next = head globalState
      
  x <- execStatef next
  case x of
    [] -> return () 
    x':xs -> runConcurrentStream execStatef (tail globalState <> (singleton x))
    -- runConcurrentStream execStatef (globalState <> (singleton x))
  {- execState reads state then applies the proper step -}
    -- Note:
     -- theres really no reason we couldnt generalize "func" :: SiteState -> SiteState for any use case
     -- that wants to use "concurrent scrape-streaming"
     -- even maybe using template haskell

  
    
  -- in     
    -- concurrentStream (globalState <> newState)

    {- recursiveApply is just an inner main -}



headStrm = undefined -- class func for how to get first item
tailStrm = undefined -- class func for how to get 2nd elem and beyond
emptyStream = undefined -- class func for how to handle empty stream -- maybe Stream superset class

findStartingPoint :: forall a b s. (DataStream s, Monoid a, a ~ s b) => a -> a
-- findStartingPoint [] = mempty --done!
findStartingPoint stream = if stream == mempty 
                           then stream -- so user needs to handle empty stream case
                           else 
                              if (headStrm stream) /= mempty
                              then stream -- returns new stream   
                              else findStartingPoint (tailStrm stream)
                             

rectifyGlobalState :: forall a b s. (DataStream s, Monoid a, a ~ s b) => a -> a
rectifyGlobalState = findStartingPoint

 
 
-- SiteState -> IO Right SiteState  --> IO SiteState --> IO GlobalState --> loop
--- ^^----------------------------------------------------------------------<<V

-- main''''' :: GlobalState -> IO GlobalState
-- main''''' = undefined

-- in theory ^^^

-- in reality, just for runConcurrentStream
  -- But this will be our `f`


-- in execState, we will GET -> Scrape -> return (Manager, SiteState) where a is some result in our monad

-- | SiteState -> IO (Manager, SiteState)

-- and manager will be needed for evaluation of siteState

-- Very much looking like GlobalState should just be StateT Manager ( ~ IO) a 
 
-- StateT Manager IO a
-- StateT GlobalState IO a

-- data GlobalState = Manager [SiteState]

-- -- | 1) pop from [SiteState]
-- -- | 2) execState with manager `on` 1) `as` SiteState_1 
-- -- | 3) Per-state analysis -> SiteState_2

-- httpLbs url manager :: 

  -- :: StateT Manager IO a 
  -- = do 
  -- gets (httpLbs url)
  
  
-- But if I use State, how do I do concurrentStream

-- is my function of type a -> b -> c -> StateT s m a --runStateT--> (s, a) ~ (Manager, SiteState)


-- Manager would need to be singular IO reference that gets passed from index to index 

-- [SiteState --> (Manager, SiteState) ,>>=Manager + Sitestate --> (Manager, SiteState) , ... , ... , ... , , , ]


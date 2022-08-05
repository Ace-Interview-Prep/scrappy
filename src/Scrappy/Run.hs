module Scrappy.Run where


import Scrappy.Requests
import Scrappy.JS


-- | Theres kinda 3 cases of scraping flows                                                                          
-- | 1) Plain link to link
-- | 2) Historied Links (such as a Map BaseUrl [RelativeUrl])
-- | 3) Defined trajectory (do this then if that works, do this, and if that works ....)


-- | The idea with this function is how can you validate that HTML is correct and not 
-- | a challenge or something if you dont try to scrape on it 

runSomeFunc :: MonadIO m => FilePath -> Url -> (Html -> Maybe a) -> MaybeT m a
runSomeFunc fp url someFunc = do
  (html, _) <- getHtmlST url 
  case someFunc html of
    Just a -> pure a
    Nothing -> do
      htmlV <- fetchVDOM url
      case someFunc htmlV of
        Just a -> pure . Just $ a
        Nothing -> do
--          encodeFile (mkFileUrl url) 
          pure . Just $ Nothing


module JSDOM where

-- | This is an experimental module to see if I could replicate enough of JSDOM to succeed
-- | -> this just works off the dir experimental/

html :: String
html = readFile "experimental/full-dom.html"


scripts = scrape (el "script" []) html

-- | I could also use stream editing to fix variable names if i have to in order to
-- | implement a given library 

x = do
  scripts' <- mapM toScript' $ fromJust scripts
  let
    script = mconcat scripts'
  streamEdit (expr html) script 
  -- This takes the DOM as an arg to replace DOM refs
  



    expr = document.getby == fromMaybe "undefined" $ scrape expr htmlPlain




fixDomRefs :: Html -> JS -> JS
fixDomRefs html js = streamEdit 



-- |Specifies element(s) within a DOM tree using various selection methods.
data Selector = ById Text
              | ByName Text
              | ByClass Text -- ^ (Note: multiple classes are not
                             -- allowed. For more control, use 'ByCSS')
              | ByTag Text
              | ByLinkText Text
              | ByPartialLinkText Text
              | ByCSS Text
              | ByXPath Text

type Args = String

data DOMRef = XPath String -- getElementByXpath
            | ById String -- getElementById 
            | ByClass String -- getElementByClassName
            | ByName String -- getElementByTagName
            | ByTag String -- like all anchor tags
            | QuerySelector Args  -- this may simplify to OneOf the others

data DOMRef' = DOMRef' Bool DOMRef -- bool could just mean getAll?

data DomUpdateType = EventListener JS -- This may need to be a recursion


-- For event listener, I write a Javascript fake interface

the implementation of EventListener's implies the impl. of Call backs but Callbacks are just JS functions
that may run If Event happens, and we can know for example that a "click" event should happen because we
are the user in simulation when we scrape.


-- | O shit wait a minute, this could be two actions
-- | 1) the callback event (if implemented)
-- | 2) The fetch and evaluation of the Index.html
click :: Clickable -> MonadJS Html 


simpleDoUpdate :: Html -> DomUpdates -> Html

-- grabWhileDoUpdate 
grabWhileDoUpdate = scrapeWithEffects

could also set up my config so that Html and Url are retrievable via config

newtype MonadJSDOM m a = MonadJSDOM { unMonadJSDOM :: StateT (Html, Url) m a }

I could test if this actually works on the tough case of an index.html


For the extreme case of:

  x = document.get(A)
  x.setProperty("prop", "val") -- syntax for demonstration purposes
  y = document.get(A) ~~ X

  -- So this get depends on the set of it previously 
  
                                                         

-- When i do the replacement I may need to also write an Element Object instantiation so that it
-- has the BS methods for add event listener 


p :: ScraperT DOMRef

p = do
  -- A <|> (XPath b)
  


jsdom can provide the full html

but fails on the HTTP request

if this happens I can therefore take that full HTML and with XHR implemented, I can generate a node script
that is fully deterministic (but doesnt handle/care about updates to the dom further)

So if I took that full HTML

  -> Html Script DOM -> StreamEditWith Script =<< DOM replacement -> then run and see if we can get the value
     of the Action




prefixed (string ".send()") {- but get the var name -} (varName <* ".responseText")
  >>= \varNameOfResponseText -> write "console.log"




but the thing is, for us to care about an XMLHTTPRequest it would have to have a significant impact on the
dom



Note too: how we could handle all DOM Updates

data HtmlOut = HtmlOut Html DOMUpdates 

data DOMUpdates = ElementScope Delta
                | WouldGetPage Url -- scrape window.location(Maybe ".href") 
  where
    ElementScope describes the range that it applies to
    Delta describes the new state



e = document.getElementById('id')
e.innerHTML = "hey"

I have to do two things to succeed on a valid e relationship
  --> provide the value for E
  --> note that this same scope, applies to the 


but also too, the only time


I know how to find XMLHttpRequest
  so I can find the name of a new one
    so i can tell how its opened ------> this yields this HTTP


domWeCareFor = someSelection $ \selection -> do
  got updates?
    Yes -> Run them as a script where the script has been edited to include the results of HTTP



effectively all Im doing is setting up a deterministic javascript expression


IDEA:
  I just hijack the XHR namespace with my own variable that does no effects and I print out at the end


-- And XHR requests 
type DomUpdate' = Elem' a -> Elem' a

data DU = Request -> Elem' a -> IO (Elem' a)


essentially the problem is I somehow need to write a nodeJS program that has access to the DOM for
gets and sets

     SO that I can determine the end HTML



Theres also no reason that the end JS script cant just declare in the global namespace some bullshit
values for any API

import XHR
declare window

run scriptWithEvaluatedDOMRefs

-- | As of rn, PromisedUpdates still have to be demanded from the Stdout parser
-- | which may be a nice design ? 
derefJS :: Script -> Html -> (Script, PromisedUpdates) -- PromisedUpdates were things like DOMRef.attribute = X
derefJS = streamEdit

I want to mock XHR myself because then I can gurantee DomUpdate logic

--> For my XHR:
  --> I could check if its a full HTML document
  --> if so this is the new DOM 


data PromisedUpdates = XHR RequestInfo
                     | Simple JSValue 

     
in general I can overwrite any JS value to help myself perform an equivalent operation to the browser




notContainedIn :: ScraperT (Elem' a) -> ScraperT a -> ScraperT a
notContainedIn e s = (e >> pure ())
                     <|> s

notContainedIn' :: ScraperT start -> ScraperT end -> ScraperT a -> ScraperT (Either () a)
notContainedIn' start end scraper = (Right <$> s) <|> (Left <$> (start >> manyTill_ anyChar end))


hijack Window + DOM + XHR -> This tells us all we need to know about the final state of the DOM
   --> Window also tells us if the DOM changed the site

   --> And then I have complete control over effects

   --> console.log any window.location 's or window.location.href 's
                     

document.getElementById('')



class Document {  
  getElementById(string) {
      updates.push(string)
  }
}

console.log(window)
console.log(document.updates)

streamEdit (window.location OR alike) ==> (console.log("location change:" <> window.location))
   or i could simply check the value of window at the end 

should I check for setting the document value to a new XHR.response.text? 

element { tag, innerHTML, attrs }

a global update would override any local update

to emulate window in full, the global variables would have to be accessible via window


for the following:

  document.innerHTML = JSVal



recurse if:

  set then get where the set is dependent on something


log an update:

  streamEdit to: e.innerHTML ==> eInnerHtml

  or:

    match@...x... = JSVal
    console.log(refName: match)

    but its always some element that is having an attribute about it changed

data StdOutGoal = StdOutGoal DOMUpdatesByScope Window (SentXhrs ?)  
    
<script>
  1
  1
  1
  1
  1
  1
  console.log(update) 
  1
  

</script>


  document.getElementById('id')

  --> scrape (el Nothing [("id", "id")])

     --> Elem' a --> ToJSON


  DOMUpdate {{{ domRef >> char "." >> someAlphaNum >> "=" >> something }}}


  2 tasks
   1) fill in any dom refs
   2) if dom ref, sets, then log name and attribute set with value


       DomUpdate (Elem' a) Attribute Value


a DomUpdate could be a scraper itself so that when you scrape a Given pattern, the `scrape` function
checks first to see if it encounters a match for the DomUpdate and if it does then it edits and resumes (ideally)

to apply a DomUpdate easily:

  parse the match, tossout
  then try to parse the OverallObjective on the replacement as a type String

     case parse whatIDesire "" (show replacement) of
         Right a -> pure a -- continue scraping
         Left err -> parserZero 



1) separate out script from DOM
2) streamEdit script to simulate DOM and Effects
3) withEffects -> scrapeWithEffects :: DomUpdates -> ScraperT a -> IO a


scrapeWithEffects :: DomUpdates -> Html -> ScraperT a -> IO (Maybe a)
scrapeWithEffects updates html scraper = do
  case findLocationChange updates of
    Just (Location (Link url)) -> do
      html <- getHtml url
      (script, dom) <- fixHtml html
      updates <- runJSWithCli script
      scrapeWithEffects updates html scraper
    Nothing -> scrapeWithLocalizedEffects updates html scraper  
      -- then I guess I would need to run JSDOM again 


data DomUpdate = DomUpdate (Elem' a -> String) (ScraperT (Elem' a))

any :: [DomUpdate] -> ScraperT String
any ((DomUpdate f pat):xs) = do
  x <- (f <$> pat) <|> (any xs)
  pure x 
  

scrapeWithLocalizedEffects :: [DomUpdate] -> Html -> ScraperT a -> Maybe [a]
scrapeWithLocalizedEffects updates html scraper = do
  refinedString <- any updates
  matches' <- case scrape scraper refinedString of
                Just matches -> pure matches 
                Nothing -> pure []
  match <- (try (baseParser parser)) <|> givesNothing <|> endStream



Another idea: all variables that have been set are available thru Object.getOwnPropertyNames(), could just log this


Set Inputs (note outputs)
Run Script (with outputs noted) With (fake window, fake XHR)
ConsoleLog then Parser StdOut -> JSAST
Update DOM

elementById id = elemParser Nothing Nothing [("id",id)]

replaceDomRef :: Html -> ScraperT String
replaceDomRef html = do
  x <- domRef
  args <- sep comma (some alphaNum)
  char ')'
  case x of
    "= document.getElementById(" -> fromMaybe "undefined" $ scrapeFirst (elementById "Z-p3") html
    "= document.getElementByXpath(" -> fromMaybe "undefined" $ scrapeFirst

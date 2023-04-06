{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Scrappy.JS where

import Scrappy.Requests (getHtml, getHtml', SessionState(..))
import Scrappy.Scrape (ScraperT, scrape, exists)
import Scrappy.Elem.SimpleElemParser (el, manyTill_)
import Scrappy.Elem.Types (Elem'(..))
import Scrappy.Links (BaseUrl, Src, Link(..), fixRelativeUrl, renderLink, LastUrl, Html)
import Scrappy.Find

import Language.Haskell.TH (recover)
import System.Which (staticWhich)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Process (CreateProcess(..), proc, readCreateProcess)
import System.IO.Temp (withTempFile)
import System.IO (hFlush)
import Data.Text.IO (hPutStr)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Control.Monad.Trans.State (StateT)
import Control.Applicative (some, optional)
import Text.Parsec (ParsecT(..), Stream, ParseError, sepBy, char, many, anyChar, parse, noneOf, string, manyTill
                   , try, alphaNum, (<|>), oneOf, parserZero)
import Control.Monad (when)
import Control.Applicative (some)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe, fromJust, catMaybes )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Data.List.Extra (isInfixOf)
import Data.ByteString.Lazy (ByteString)
import Data.String.Utils (strip)

import Text.RawString.QQ (r)

import Data.Aeson.TH (defaultOptions, deriveJSON)


-- Heres the solution:

  
--   I stream edit the collection of JS scripts (so first I concat them)

--   I stream edit, changing parsed DOMRefs into expressions that are effectively
--   fancy rewirings of \spec -> scrape elemParser spec






-- | IDEA !!!!

 --  to extend to references from the DOM such as an onclick, I can literally just append this to runVDOMWith $ JS domRef

  -- <div onclick="f()"> --> runVDOMWith dom (JS $ getOnClick divElem) 

  --  I can probably even do this in a safe manner where this only needs
  --  the run script option of "external" or whatevs in JSDOM







-- | TL;DR: This is a way to build a deterministic outSet
-- | ... which we can build with a Map of key values from the last run
data JSAST = JSAST { inSet :: M.Map Name JSVal -- could include dom = ... 
                   -- , outSet :: [Name]
                   , script' :: JS
                   -- ^ FunctionDeclarations data type is really a subcategory of this
                   -- ^ this just is meant to rep that as well as setting variables
                   -- ^ although we could run function declaractions
                   -- ^ or make a Module data structure 
                   }


-- | In how we write the script, there is no reason that we need to 
data JSInteraction = Get Name
                   | GetByExpr Name JSExpr
                   -- ^ Name is new name 
                   | Set Name JSVal
                   | SetGet Name JSVal
                   -- ^ Can be any name 


newtype MonadJS' a = MonadJS' a

-- | We could also just only parse when we need to get back into the haskell context
-- | I dont know if I need the reader part
newtype MonadJS m a = MonadJS { runMonadJS :: StateT JSAST m a } 



type Attributes = M.Map String String
data Script = Script Attributes Text deriving (Show, Eq) 


instance Semigroup Script where
  (Script _ b) <> (Script _ b2) = (Script mempty (b <> ";\n" <> b2))

instance Monoid Script where
  mempty = Script mempty mempty 

--- toScript is not working

-- -- | I wouldnt need this at all tho for 
-- toScript :: SessionState sv => sv -> BaseUrl -> Elem' a -> IO (Maybe Script)
-- toScript mgr baseUrl (Elem' _ atrs _ inner) = do
--   let src = M.lookup "src" atrs
--   case src of
--     Just a -> Just <$> toScript' mgr baseUrl (fromMaybe "" src) (strip inner)
--     Nothing -> pure Nothing 
-- --  print src


-- could do:

--   fetch normally
--   when (not . exists $ target) $ do runVDOMFetch lastUrl
--      when this == none $ do endsite 


--getHtmlPromise :: Link -> Html


--           writeFile url html
--           writeFile (url <> "2") htmlV

-- someFunc :: Html -> Maybe a

        



data Fail = Fail { url :: Link
                 , html :: Html
                 } deriving (Show) --- , Generic)






      

-- This is fine cuz I'll probably eliminate Attrs field in script 
toScript' :: SessionState sv => sv -> LastUrl -> Src -> String -> IO Script
toScript' sv (Link baseU) src inner
  | null inner && (isInfixOf baseU src) = getHtmlST sv (Link src) >>= (\(scriptSrc, _) -> pure $ Script mempty $ pack scriptSrc) 
  | null inner && (src /= "") = do
      -- getHtml with rectified URL then set as body
      let url = fixRelativeUrl (Link baseU) src
      (scriptSrc,_) <- getHtmlST sv (Link url)
      pure $ Script mempty $ pack inner
  | otherwise = pure $ Script mempty $ pack inner

  -- case (null inner && (src /= Nothing)) of
  --   True -> do
  --     -- check if src is relative or absolute
  --     if 
  --     (scriptBody, _) <- getHtmlST mgr $ fromJust src
  --     pure $ Script atrs $ pack scriptBody 
  --   False ->
  --     pure $ Script atrs $ pack inner

-- getHtmlRaw = getHtmlST

-- getHtmlFinal :: SessionState sv => sv -> String -> IO Text
-- getHtmlFinal mgr url = do
--   (html, _) <- getHtmlST mgr url
--   -- print html
--   let
--     Just scriptElems = scrape (el "script" []) html
--     -- scripts' = scripts >>= (\ss -> fmap toScript ss)
--   scripts <- mapM (toScript mgr) scriptElems
--   let
--     Script _ firstScript = head scripts
--     Script _ theScript = mconcat scripts 
--   runScript firstScript--theScript
  
  
-- data HTML = Raw Html | TreeH (Forest HTML) -- theoretically speaking  



-- mkSession :: IO Session
-- mkSession = newSession defaultConfig 

-- -- | Doesnt work at all 
-- doJS :: IO ()
-- doJS = do
--   sesh <- mkSession
--   print "got here"
--   (result :: ByteString) <- eval sesh [js| 1+1; |]
    
--   print result
--   pure ()



 -- -get node path from environment (set by nix prior)
 -- -withTempFile run script in node and redirect standard output back into local Monad
 -- -output == document.body ->then: decode into haskell String ~ Html

 --  **** AS a test though:; 1 + 1 ;;; function () { return 1; } ;;; then above 3rd step 


type Body = Text

-- getNodePath :: 


-- -- | Nice pattern : get environment variable and execute the specified command   
-- procCli :: FilePath -> [String] -> CreateProcess
-- procCli workingDirectory args =
--   addEnvironmentVariable ("CKB_CLI_HOME", workingDirectory) $ proc ckbCliPath args
--   -- ::  (String, String) -> CreateProcess -> CreateProcess
  
-- | Should not be needed with static which ? 
-- addEnvironmentVariables :: [(String, String)] -> CreateProcess -> CreateProcess
-- addEnvironmentVariables args cp =
  -- cp { env = env cp <> Just args }

  

-- | To avoid file collisions I could create a hash that the file is named with

-- | Falls back to Nix if it cannot find the path
-- | Compilation will fail if this does 
nodePath :: FilePath
-- nodePath = $(recover (staticWhichNix "node") (staticWhich "node")) 
nodePath = --- $(staticWhich "node")
 $(staticWhich "node")



type JSVal = String -- todo: Text

runNodeJS :: Script -> IO JSVal 
runNodeJS = runJSWithCli 

runJSWithCli :: Script -> IO JSVal 
runJSWithCli (Script _ script) = do
  withTempFile "." "index.js" $ \fp handle -> do
    print $ "fp:" <> fp
    hPutStr handle $ script
    hFlush handle 
    readCreateProcess (proc nodePath [fp]) []



-- f = do
--   -- req <- parseRequest "https://hoogle.haskell.org"
--   -- mgr <- newManager tlsManagerSettings
--   download  
--   readCreateProcess (proc nodePath ["index.js"]) []
--   where
--     download = (writeFile "index.js") . unpack . unScript . runVirtualDOM . pack =<< getHtml' "https://hoogle.haskell.org" 



mkJSDOMAct :: Html -> JS
mkJSDOMAct indexHtml = undefined


unScript :: Script -> Text
unScript (Script _ s) = s

newtype JS = JS { unJS :: Text }

instance Semigroup JS where
  JS script <> JS runNext = JS $ script <> ";\n" <> runNext 

instance Monoid JS where
  mempty = JS ""

-- test = do
--   mgr <- newManager tlsManagerSettings
--   let baseUrl = "https://hoogle.haskell.org"
--   (indexHtml, _) <- getHtmlST mgr baseUrl 
--   let Just scripts = scrape (el "script" []) indexHtml
--   scripts' <- mapM (toScript mgr baseUrl) scripts
--   -- x <- runJSWithCli $ Script mempty "console.log('hello world');\nconsole.log('hello again')"
--   --print "head"
--   --print $ head scripts'
--   let finalScript = mconcat scripts'
--   -- print finalScript 
--   x <- runJSWithCli finalScript
--   print "ran script"
--   -- print $ take 5 x
--   pure ()


-- it is clear that in order for us to get values back into haskell they have to first be directed to stdout
-- so if we were to do any execution, it would be pointless if that were not wrapped with console.log() or
-- the results or variables were not redirected to some JSON file/struct 


-- buildJSCall :: 


-- | All this is, is a signal to me for what to console.log
-- | 
data JSCall = JSCall JS ReturnVars 

type ReturnVars = [Text]



-- data JSCall' = JSCall' (Interface a b) (a -> b) 



  
test2 = do
  let script = Script mempty $ "function f() { return 1; }"
  -- runJSWithCli script
  -- print "withScript"
  -- str <- runJSWithCli $ withScript script (JS "console.log(f() + f())")
  -- print $ "string:" <> str
  str2 <- runJSWithCli $ withScript script (JS "f()")
  print str2

-- | Using a JS Source that may have some functions, run some command at the end which uses these scripts
-- | This means any function declarations in the Script are in scope for command
withScript :: Script -> JS -> Script 
withScript (Script a script) (JS command) = Script a (script <> ";\n"
                                                      <> "console.log(" <> command <> ")")


-- | runVDOMWith dom $ JS $ "let x=1" <> "console.log(x)"


-- IDEA!!!
-- | haskellFunctionThatMakesJS arg1 arg2 arg3 = writeJSFunc arg1 arg2 arg3 ~~~ "let x=1; y=3; v=4; function f(x,y,v) { logic -> return o }; console.log(f(x,y,v))" 

-- | One way that we could actually do this, is by making the implementor write the logic of the function for javascript
-- | 

-- | f :: JSFunc -> A -> B -> C -> JS 
-- | f jsFunc a b c = undefined

-- IDEA!!!! WE could also use the outputted JSVals to create a recursive interface
                                 -- that creates DOM (but why would I?) 

returnJSVal :: ParsecT s u m a -> Int -> [JSVal] -> a
returnJSVal = undefined

findJSVal :: ParsecT s u m a -> [JSVal] -> a
findJSVal = undefined



-- -- | Would be really cool if we could somehow state the return type
-- -- | And i feel like we can:
-- -- |
-- -- |  Script a = Script Text -- where a is a Phantom type that must be parsed into
-- -- |
-- -- | further :     Script (Maybe a) = Script Text --- > Conceptually at least
-- -- |
-- mkVDOMScript :: Text -> Script
-- mkVDOMScript htmlTmpFilePath = Script mempty
-- --                     $ "let html= Buffer.from(`" <> (pack . (filter (\x -> (x /= '\n') || (x /= '\t'))) . unpack $ html) <> "`, 'utf8');"
-- --                     $ "let html= Buffer.from(, 'utf8');"
--                      $ "let fs = require('fs');"
--                      <> "var text = fs.readFileSync(\"" <> htmlTmpFilePath <> "\").toString('utf-8');"
--                      <> "console.log(text);"
--                      -- <> "let jsdom = require('jsdom');"
--                      -- <> "let dom = new jsdom.JSDOM(html);"
--                      -- <> "console.log(dom.serialize())"





-- | There's probably an implementation which uses laziness to our advantage
-- | where we are only calling node on functions where we have to 
runVDOM :: BaseUrl -> Html -> ExceptT ParseError IO Html
runVDOM baseUrl htmlString = fst <$> runVDOMWith baseUrl htmlString mempty 

  
-- | There's probably an implementation which uses laziness to our advantage
-- | where we are only calling node on functions where we have to 
runVDOM' :: Html -> ExceptT ParseError IO Html
runVDOM' htmlString = fst <$> runVDOMWith (Link "") htmlString mempty 



-- withTempFile "." "index.html" $ \htmlFPath handle -> do
--   hPutStr handle $ pack htmlString
--   hFlush handle
--   runJSWithCli $ mkVDOMScript htmlFPath
--   where
--     mkVDOMScript fp = Script mempty
--                      $  "let fs = require('fs');"
--                      <> "let jsdom = require('jsdom');" 
--                      <> "var text = fs.readFileSync(\"" <> (pack $ filter (\x -> (x /= '\n') || (x /= '\t')) $ fp) <> "\").toString('utf-8');"
--                      <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\" });"
--                      <> "console.log(dom.serialize());"
  

-- newtype VirtualT m a = StateT DOM m a 


-- type DOM = (Html, JSAST) 



-- for this to work properly I need to write a JS template that takes as input the prevous state and applies
-- some js to it like the 3 main cases

-- \x -> x -- just grab it
-- \_ -> 1 -- just set it
-- \x -> x + 1 -- set it with itself as input for the output 





newtype FunctionDeclarations = FuncDecls { decls :: Text }

data JSType = JSONString String | JSNumber String | JSString String


showJSType :: JSType -> Text 
showJSType (JSONString str) = pack str
showJSType (JSNumber str) = pack str
showJSType (JSString str) = "\"" <> (pack str) <> "\"" 

f' :: JSVal -> JSType 
f' val
  | length (filter (== '{') val) == length (filter (== '}') val) = JSONString val
  | exists (oneOf (['A'..'Z'] <> ['a'..'z'])) val = JSString val
  | length val == (length $ filter (\x -> elem x ['0'..'9']) val) = JSNumber val  
  | otherwise = error "unknown javascript data type" 

-- Writing the state of our map representing JSVal's is just set at the beginning

  
-- writeAST :: Map String String -> JS 
-- writeAST map = fmap f $ toList map
--   where f k v = "let " <> k <> " = " <> v <> ";"    


-- -- | JSInteraction will be used as input to writing the final JS 
-- writeAST :: M.Map Name (Maybe JS) -> [JSInteraction]
-- writeAST map = fmap f $ toList map
--   where f k Nothing = "console.log(" <> k <> ");" -- best to run at the end and grab 
--         f k (Just a) = GetSet 


type Name = String 

-- -- (SET, GET)
-- writeJSInteraction :: JSInteraction -> (JS, JS)
-- writeJSInteraction (Get name) = getJSVal name
-- writeJSInteraction (Set name valExpr) = setJSVal 
-- writeJSInteraction (SetGet ) = ""
-- writeJSInteraction (GetByExpr ) = ""

-- To grab all, requires Scraping and to Set any requires stream editing

-- streamEdit input <> get


-- streamEditGrab :: String -> (String, [Name])


-- streamEditGrab str >>= \(script, names) -> script <> (mconcat $ fmap getJSVal names)



jsVarName :: ScraperT Name 
jsVarName = do
  (try $ string "let ") <|> (try $ string "var ") <|> (try $ string "const ")
  many (char ' ')
  name <- some $ alphaNum
  manyTill_ (char ' ') (char '=')
  many (char ' ')
  pure name

-- :t f ast ~~~= function for streamEdit




-- parseOnly (convertToAtto json) (



-- | If we can parse JSON (which we can) then we have Objects
-- | For now though, I will just takeTill ;
-- | and then reset 
jsSetVariable :: [(Name, JSVal)] -> ScraperT Name 
jsSetVariable setters = do
  name <- jsVarName
  when (not $ elem name $ fmap fst setters) parserZero 
  (value,_) <- manyTill_ anyChar (char ';')
  pure name
  --_ <- (JSON <$ object)
  --     <|> 
  --where
  --  object = char '{' 

overrideScript :: [(Name, JSVal)] -> JS -> JS 
overrideScript overrides (JS js) = JS $ pack $ streamEdit (jsSetVariable overrides) (f $ overrides) (unpack js)
  where 
    -- | Temporarily String 
    f :: [(Name, JSVal)] -> Name -> String
    f ast name = let v = fromJust $ lookup name ast
                 in unpack $ unJS $ setJSVal name v 


-- | Just keeps track of how many start/opens 
looseBracketed :: ScraperT start -> ScraperT end -> ScraperT String
looseBracketed = undefined 


-- (shit, thing) <- manyTill_ shitIDC thing
-- (<>) <$> (pure $ shit <> (f thing)) <*> continue 


-- | This will be collected into the map 
getJSVal :: Name -> JS 
getJSVal jsName = JS $ "console.log(`" <> (pack jsName) <> "`, " <> (pack jsName) <> ");"

-- | Bool if String
setJSVal :: Name -> JSVal -> JS 
setJSVal jsName jsVal = JS $ "let " <> (pack jsName) <> " = " <> (showJSType . f' $ jsVal) <> ";"

                               

-- NOTE: remember to test Higher Kinded datatypes that are semigroup-ish

mkScriptInterop :: Script -> [JSInteraction] -> Script
mkScriptInterop script interactions = undefined



-- I could also do one that grab





-- | ^^ SO: getAST :: Parser a -> Name -> a

-- -- | Never takes from State, only copies it so it will stay available in the JS context 
-- getAST :: Stream s m Char => ParsecT s u m a -> Name -> a
-- getAST = undefined 

setAST :: Show a => Name -> a -> MonadJS' () 
setAST name val = undefined

-- Note too that, lets say we want to carry forward past vars

-- we could even write this AST at the beginning of the script

-- we also need a way to trim the map of jsVals

-- | Declare and save what you want to carry from last run 
orderJS :: M.Map String String -> MonadJS' ()
orderJS = undefined

-- | delete keys
resetSomeJS :: M.Map String String -> MonadJS' ()
resetSomeJS = undefined




type JSFunc = JSVal -> JSExpr
-- type JSFunc' a b = a -> MonadJS b 


-- | Must not start with let 
newtype JSExpr = JSExpr { express :: Text }


-- 1)
exampleJSFunc :: JSVal -> JSExpr
exampleJSFunc jsval = JSExpr $ pack jsval 


fromJSFunc :: JSFunc 
           -> Name
           -> JS
          -- ^ this could have 'a' as a phantom type that enforces type on the return into haskell
          -- but that would also disallow (a -> b) and only (a -> a)
          -- But this also means that this should be a Monad to be a full fledged FFI 
fromJSFunc jsExpr name = JS $ 
                       pack $ "let " <> name <> "=" <> (unpack $ express $ jsExpr name) <> ";\n"
                       <> "console.log(" <> name <> ");"
  

-- runVDOMWith html $ compileJS userMadeJS "someName"

-- Tuple a => a -> Args ~ String 


-- -- | I Could expand this to use a GADT that can carry forward simple values
-- -- | there is also a way that if I was to mutate the JSAST recursively
-- -- | that I could carry forward plain values
-- data JSAST = JSAST { script :: Text
--                    , ast :: Map Name (Maybe JS)
--                    }
             

-- data  JSAST = JSAST { varDecls :: [(Name, JSVal)]
--                       -- ^ overrides 
--                     , out :: [Name]
--                     , scriptyy :: Script
--                     , domUpdates :: Maybe [JS]
--                       -- ^ this is just to test an idea, would only apply to DOM environment case
--                     }

-- with setting variables + a script we may need to streamEdit the script
-- Actually for now we could just set lets to vars and this would be the most reliable
-- interop method



                   
-- updateAST :: JSAST -> JST IO JSAST 
--                   | SetWithJSExpr Name 

-- runVDOMST = updateAST . runVDOMWithAST 

-- JSAST -> JSInteraction

class MonadJS'' m where
  runJS' :: JS -> m () -- Update is in state 
  getAST :: m JSAST
  setAST' :: JSAST -> m () 

-- | In order for a 
newtype JST m a = JST { runJST :: StateT JSAST (ExceptT JSError m) a }

-- | TODO(Galen): add in forall m. MonadIO m => m
newtype JSDomT m a = JSDomT { runJSDomT :: StateT (Html, JSAST) (ExceptT JSError m) a }

type JSError = String 

-- | Stateful runVDOM

runVDOMST :: JS -> JSDomT IO (M.Map Name JSVal)
runVDOMST xs = undefined



-- -- | You have to specify get of a variable for Haskell to be aware of it 
-- runJS :: JS -> MonadJS m ()
-- runJS js = do
--   let getAST = undefined
--   ast <- getAST 
--   runJSWithCliStatefully $ mkScript ast js 
--   where
--     mkScript = undefined



-- | Would grab all variables in top level of JS
-- | and would do so by scraping all top level variable declarations
runGrabJS :: JS -> MonadJS' ()
runGrabJS js = undefined

runJSWithCliStatefully = undefined

-- | Scripts can reference the DOM by using the name 'dom' 
-- | Could make this a Monad if it is worth it
-- | Currently, you need to console.log the return value or this runs but doesnt return
runVDOMWithAST :: Html -> JSAST -> [JSInteraction] -> ExceptT ParseError IO (Html, JSAST)
runVDOMWithAST indexHtml (JSAST inSet (JS js)) userInput = withTempFile "." "index.html" $ \htmlFPath handle -> do
  let
    inSetToSetGet = fmap (\(a,b) -> SetGet a b) $ M.toList inSet
    mkBoilingPot = inSetToSetGet <> userInput
    setters = catMaybes $ fmap filterSets mkBoilingPot
    getters = catMaybes $ fmap filterGets mkBoilingPot
    -- User input would override auto input
  
  liftIO $ hPutStr handle $ pack indexHtml 
  liftIO $ hFlush handle 
  rawJSVal <- liftIO $ runJSWithCli $ mkVDOMScript htmlFPath setters getters
  (html, mappy) <- except $ parse jsStdOutParserAST "" rawJSVal
--  let
--     out
  pure (html, JSAST mappy (JS js)) 
  where
    mkVDOMScript fp setIn getOut = Script mempty
      $  "let fs = require('fs');"
      <> "let jsdom = require('jsdom');" 
      <> ("var text = fs.readFileSync(\"" <> (pack fp) <> "\").toString('utf-8');")
      <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\" });"
      <> (unJS $ mconcat setIn) 
      <> (pack ['\n'])
      <> js
      <> (pack ['\n'])
      <> (unJS $ mconcat getOut) 
      <> (pack ['\n'])
      <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
      <> "console.log(dom.serialize());"


-- We could expand JSAST by adding "modules" which are just blocks of JS code which have their function
-- definitions 



-- mkFFI :: ExecutableName -> AST

-- I could also simplify my life by directly asking for the JS expression which may use
-- your predefined scripts and I can do this to make it Lazy where

--                                        I just parse the expression which has a definite format and
--                                        I can invalidate as well as having a clear reference system 


filterSets :: JSInteraction -> Maybe JS
filterSets (Get name) = Nothing
filterSets (Set name value) = Just $ setJSVal name value
filterSets (SetGet name value) = Just $ setJSVal name value
filterSets (GetByExpr _ _) = Nothing


subscribe :: Name -> MonadJS m a
subscribe = undefined

-- | When STDOUT writes the results, they are separated by \n char
filterGets :: JSInteraction -> Maybe JS
filterGets (Set _ _) = Nothing
filterGets (Get name) = Just $ getJSVal name
filterGets (SetGet name val) = Just $ getJSVal name
filterGets (GetByExpr newName exprJS) = Just . JS $ "console.log(\"newName:\", (" <> (express exprJS) <> "));"
                               

-- The reason for JS Interaction is to compile our systems work with the users input
-- and both sources must follow the same format


-- GOAL: jsStdOutParserAST :: Stream s m Char => ParsecT s u m (Html, JSAST) 
jsStdOutParserAST :: Stream s m Char => ParsecT s u m (Html, M.Map String String) 
jsStdOutParserAST = do
  (jsvals, _) <- manyTill_ (f <* (char '\n')) (string "!@#$%^&*()")
  html <- many anyChar
  pure (html, M.fromList jsvals)
  where
   --  f = (,) <$> (manyTill alphaNum (try $ string "|>")) <*> (string "|>" >> (some $ noneOf ['\n']))
    f = (,) <$> (manyTill alphaNum (try $ string ":")) <*> (string ":" >> (some $ noneOf ['\n']))

                   
-- | Scripts can reference the DOM by using the name 'dom' 
-- | Could make this a Monad if it is worth it
-- | Currently, you need to console.log the return value or this runs but doesnt return
runVDOMWith :: BaseUrl -> Html -> JS -> ExceptT ParseError IO (Html, [JSVal])
runVDOMWith baseUrl indexHtml (JS js) = withTempFile "." "index.html" $ \htmlFPath handle -> do
  liftIO $ hPutStr handle $ pack indexHtml 
  liftIO $ hFlush handle
  liftIO $ print $ pack $ renderLink baseUrl
  rawJSVal <- liftIO $ runJSWithCli $ mkVDOMScript htmlFPath
  except $ parse jsStdOutParser "" rawJSVal 
  where
    -- mkVDOMScript fp = Script mempty
    --   $  "let fs = require('fs');"
    --   <> "let jsdom = require('jsdom');" 
    --   <> ("const text = fs.readFile(\"" <> (pack fp) <> "\").toString('utf-8');")
    --   <> "const rLoader = new jsdom.ResourceLoader({ strictSSL: true, userAgent: \"Mellblomenator/9000\",});"
    --   <> "const virtualConsole = new jsdom.VirtualConsole();"
    --   <> "virtualConsole.sendTo(console, { omitJSDOMErrors: true });"
    --   <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\", resources: rLoader"
    --   <> ", url: \"" <> (pack baseUrl) <> "\", virtualConsole});"
    --   <> js
    --   <> (pack ['\n'])
    --   <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
    --   <> "console.log(dom.serialize());"

    mkVDOMScript fp = Script mempty
      $  "let fs = require('fs');"
      <> "let jsdom = require('jsdom');" 
      <> ("fs.readFile('" <> (pack fp) <> "', function(err, text) {")
      <> "const rLoader = new jsdom.ResourceLoader({ strictSSL: false, userAgent: \"Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0\",});"
      <> "const virtualConsole = new jsdom.VirtualConsole();"
      <> "virtualConsole.sendTo(console, { omitJSDOMErrors: false });"
      <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\", resources: rLoader"
      <> ", url: \"" <> (pack . renderLink $ baseUrl) <> "\", virtualConsole});"
      <> js
      <> (pack ['\n'])
      <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
      <> "console.log(dom.serialize());"
      <> "});"


                   
-- | Scripts can reference the DOM by using the name 'dom' 
-- | Could make this a Monad if it is worth it
-- | Currently, you need to console.log the return value or this runs but doesnt return
fetchVDOMWith :: Link -> JS -> ExceptT ParseError IO (Html, [JSVal])
fetchVDOMWith (Link url) (JS js) = do
  rawJSVal <- liftIO $ runJSWithCli $ mkVDOMScript
  except $ parse jsStdOutParser "" rawJSVal 
  where
    mkVDOMScript = Script mempty
      $  "let fs = require('fs');"
      <> "let jsdom = require('jsdom');" 
--      <> ("fs.readFile('" <> (pack fp) <> "', function(err, text) {")
      <> "const rLoader = new jsdom.ResourceLoader({ strictSSL: false, userAgent: \"Mozilla/5.0 (X11; Linux x86_64; rv:84.0) Gecko/20100101 Firefox/84.0\",});"
      <> "const virtualConsole = new jsdom.VirtualConsole();"
      <> "virtualConsole.sendTo(console, { omitJSDOMErrors: false });"
      <> "let options = { runScripts: \"dangerously\", resources: rLoader, virtualConsole};"
--      <> "let dom = new jsdom.JSDOM(text, { runScripts: \"dangerously\", resources: rLoader, virtualConsole});"
      <> "jsdom.JSDOM.fromURL(\"" <> (pack url) <> "\", options).then(document =>"
      <> js
      <> (pack ['\n'])
      <> "console.log('!@#$%^&*()');" -- seperate result: is sufficiently unlikely to occur 
      <> "console.log(dom.serialize());"
      <> ")"
      
--      <> "});"


-- type Url = Text -- String
-- getHtmlJS :: Url -> IO Html 
-- getHtmlJS url = undefined $ "JSDOM.fromURL(\"" <> url <>  "\", options).then(dom => {console.log(dom.serialize());});"
-- -- | TODO(galen): 
-- -- const cookieJar = new jsdom.CookieJar(store, options);
-- -- const dom = new JSDOM(``, { cookieJar });


-- in the above, if we knew, what they wanted to return, then we could allow a javascript expression


-- return = [ "a", "myVar" ]


-- return = [ ("a", Nothing)
--          , ("myVar", "1+1") -- on the left is a statement which can call any variables in the JS scope 
--          ]
         
-- >> console.log("a:" <jsConcat> a) -- this would return undefined if a doesnt exist
-- >> console.log("myVar:" <jsConcat> myVar)  -- we know this exists



jsStdOutParser :: Stream s m Char => ParsecT s u m (Html, [JSVal]) 
jsStdOutParser = do
  (jsvals, _) <- manyTill_ ((some $ noneOf ['\n']) <* (char '\n')) (string "!@#$%^&*()")
  html <- many anyChar
  pure (html, jsvals)
      -- (fst ,) <$> (manyTill_ ((some $ noneOf ['\n']) <* (char '\n')) (string "!@#$%^&*()")) <*> many anyChar 
  
                   
-- scrape (string "|>" >> manyTill_ anyChar (string "|>")) string


type Prefix' = String
{-# DEPRECATED scrapeBracketed "experimental, first attempt" #-}
scrapeBracketed :: Prefix' -> ScraperT a -> Html -> Maybe [a]
scrapeBracketed pre scraper html = mconcat <$> scrape (string pre >> manyTill scraper (string pre)) html



type Prefix = String 
scrapePrefixed :: Prefix -> ScraperT a -> Html -> Maybe [a]
scrapePrefixed pre scraper html = scrape (string pre >> scraper) html




-- | TODO(galen): set infix to be lower than <>
-- | TODO(galen): check if last character of a is ; 
(<&>) :: JS -> JS -> JS
(<&>) (JS a) (JS b) = JS $ a <> ";\n" <> b


-- | Specialized to only taking the last n and put them in the proper order 
takeJSOut :: Int -> [JSVal] -> [JSVal]
takeJSOut n vals = reverse $ take n vals 


-- looseSep sep p = do
--   optional sep
--   liftA2 (<>) p (looseSep sep p)  
  



  
  -- x <- some $ noneOf ['']
  -- optional (char '\n')
  -- xs <- maybeJSvals
  -- case xs of
  --   Just vals -> 
  -- pure (x:xs)
  
  

-- scrape (noneOf 
 
-- jsvals :: Stream s m Char => ParsecT s u m [JSVal]
-- jsvals = do
--   x <- some $ noneOf ['\n']
--   optional (char '\n')
--   xs <- maybeJSvals
--   case xs of
--     Just vals -> 
--   pure (x:xs)
  


-- | This doesn't determine the result out or type out
-- | But instead just breaks up on \n to give results 
parseJSOut :: JSVal -> [JSVal]
parseJSOut j = reverse $ fromRight undefined $ parse jsvals "" j
  where 
    jsvals = sepBy (some $ noneOf ['\n']) (char '\n')

-- | Right now its a list of 1 
runScript :: Script -> IO [JSVal] 
runScript script = runJSWithCli script >>= (\str -> pure $ takeJSOut 1 $ parseJSOut str)


runScriptText :: ScriptText -> IO [String]
runScriptText script = runJSWithCliText script >>= (\str -> pure $ takeJSOut 1 $ parseJSOut str)


runJSWithCliText :: ScriptText -> IO String 
runJSWithCliText (ScriptText script) = do
  withTempFile "." "index.js" $ \fp handle -> do
    hPutStr handle $ script
    hFlush handle 
    readCreateProcess (proc nodePath [fp]) []




-- -- | I could build on this by having a utility scraping function which tries a scraper
-- -- | and fallsback to runJSDOM .then(run scraper) in the case of a failure
-- runJSDOM :: Html -> IO Html
-- runJSDOM indexHtml = do
--   let scrMain =
--         "let htmlIn = `" <> (pack indexHtml) <> "`;"
--         <> "let dom = new jsDomLib.JSDOM(htmlIn, { runScripts: \"dangerously\" }) "
--       script = Script mempty $ "let jsDomLib = require('jsdom');" <> scrMain
--   fmap head $ runScript $ withScript script (JS "console.log(dom.serialize()
                                            
-- Scrape all script elems
-- Fetch if src attribute not null and set HTTP body to the key of the data struct
-- Append all script bodies together

-- 1. scrape (el "script" []) html



-- -- | I could build on this by having a utility scraping function which tries a scraper
-- -- | and fallsback to runJSDOM .then(run scraper) in the case of a failure
-- runJSDOMText :: Text -> IO Html
-- runJSDOMText indexHtml = do
--   let scrMain =
--         "let htmlIn = `" <> (indexHtml) <> "`;"
--         <> "let dom = new jsDomLib.JSDOM(htmlIn, { runScripts: \"dangerously\" }) "
--       script = ScriptText $ "let jsDomLib = require('jsdom');" <> scrMain
--   fmap head $ runScriptText $ withScriptText script (JS "console.log(dom.serialize())")
-- -- Scrape all script elems
-- -- Fetch if src attribute not null and set HTTP body to the key of the data struct
-- -- Append all script bodies together


-- | Using a JS Source that may have some functions, run some command at the end which uses these scripts
-- | This means any function declarations in the Script are in scope for command
withScriptText :: ScriptText -> JS -> ScriptText
withScriptText (ScriptText script) (JS command) = ScriptText (script <> ";\n"
                                                              <> "console.log(" <> command <> ")")


data ScriptText = ScriptText Text





-- newTest = runJSDOM htmlTest


-- -- TODO(galen): change return document to just being done in the JSM monad 
-- runScript :: Text -> IO Text
-- runScript script = do
--   ctxRef <- mkContextRef
--   runJSaddle ctxRef $ valToText =<< (eval ("1+1" :: Text) ) -- $ finallyGetDocument script)
--   where
--     finallyGetDocument :: Text -> Text
--     finallyGetDocument script = script <> "; return document;"


--- TAKEN FROM JSaddle run



-- runJavaScript :: (Batch -> IO ()) -> JSM () -> IO (Results -> IO (), Results -> IO Batch, IO ())
-- runJavaScript sendBatch entryPoint = do

-- mkContextRef :: IO JSContextRef
-- mkContextRef = do 
--     contextId' <- randomIO
--     startTime' <- getCurrentTime
--     recvMVar <- newEmptyMVar
--     lastAsyncBatch <- newEmptyMVar
--     commandChan <- newTChanIO
--     callbacks <- newTVarIO M.empty
--     nextRef' <- newTVarIO 0
--     finalizerThreads' <- newMVar S.empty
--     animationFrameHandlers' <- newMVar []
--     loggingEnabled <- newIORef False
--     liveRefs' <- newMVar S.empty
--     let ctx = JSContextRef {
--           contextId = contextId'
--           , startTime = startTime'
--           , doSendCommand = \cmd -> cmd `deepseq` do
--               result <- newEmptyMVar
--               atomically $ writeTChan commandChan (Right (cmd, result))
--               unsafeInterleaveIO $
--                 takeMVar result >>= \case
--                 (ThrowJSValue v) -> do
--                   jsval <- wrapJSVal' ctx v
--                   throwIO $ JSException jsval
--                 r -> return r
--           , doSendAsyncCommand = \cmd -> cmd `deepseq` atomically (writeTChan commandChan $ Left cmd)
--           , addCallback = \(Object (JSVal ioref)) cb -> do
--               val <- readIORef ioref
--               atomically $ modifyTVar' callbacks (M.insert val cb)
--           , nextRef = nextRef'
--           , doEnableLogging = atomicWriteIORef loggingEnabled
--           , finalizerThreads = finalizerThreads'
--           , animationFrameHandlers = animationFrameHandlers'
--           , liveRefs = liveRefs'
--           }
--     pure ctx 



-- wrapJSVal' :: JSContextRef -> JSValueReceived -> IO JSVal
-- wrapJSVal' ctx (JSValueReceived n) = do
--     ref <- liftIO $ newIORef n
--     when (n >= 5 || n < 0) $
-- -- #ifdef JSADDLE_CHECK_WRAPJSVAL
--      do lr <- takeMVar $ liveRefs ctx
--         if n `S.member` lr
--             then do
--                 putStrLn $ "JS Value Ref " <> show n <> " already wrapped"
--                 putMVar (liveRefs ctx) lr
--             else putMVar (liveRefs ctx) =<< evaluate (S.insert n lr)
-- -- #endif
--         void . mkWeakIORef ref $ do
--             ft <- takeMVar $ finalizerThreads ctx
--             t <- myThreadId
--             let tname = pack $ show t
--             doSendAsyncCommand ctx $ FreeRef tname $ JSValueForSend n
--             if tname `S.member` ft
--                 then putMVar (finalizerThreads ctx) ft
--                 else do
--                     addThreadFinalizer t $ do
--                         modifyMVar (finalizerThreads ctx) $ \s -> return (S.delete tname s, ())
--                         doSendAsyncCommand ctx $ FreeRefs tname
--                     putMVar (finalizerThreads ctx) =<< evaluate (S.insert tname ft)
--     return (JSVal ref)



-- addThreadFinalizer :: ThreadId -> IO () -> IO ()
-- addThreadFinalizer t@(ThreadId t#) (IO finalizer) =
--     IO $ \s -> case mkWeak# t# t finalizer s of { (# s1, _ #) -> (# s1, () #) }



deriveJSON defaultOptions ''Fail












  

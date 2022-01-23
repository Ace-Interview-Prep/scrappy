-- |

module RunJS where

















-- Overall flow of an Expression is meant to be lazy like haskell,
-- Program takes a list of JS expressions ~~ a Batch
-- and puts them to a JS normal form so that we have one javascript expression

-- We could ideally fully use the power of laziness if we have an analysis
-- of the JS function (or expression with function, f) that backtracks from the output to
-- see if the syntax clearly indicates whether or not a value OR expression--> value
-- never interacts with the eventual output, and based on such result, discard the naming
-- reference so that we never even call that function



mkContextRef :: IO JSContextRef

runJS :: String -> IO String -- or whatever


type JSExpression = Tree JSFunction

-- but how would this work with cases like an Angular or React VDOM

-- Is there a way we can determine the VDOM?

type VDOM = JSScript -> IO Html
type OrVDOM = JSScript
data VDom = VDom JSScript Framework

-- Build all the functions in the script
memoize :: JSScript -> Forest JSFunction

-- | Given JS entry point, determine the expressions the script would run
-- | Not including the creation of functions
-- | Maybe also not instantiating the variables
getMainLogic :: JSScript -> JSFunction
-- | This would be most likely useful for a VDOM scenario
-- | Main is always going to be an ordered list of Expressions/Actions where an Action may have an Expression
-- | Ideally this returns an ordered list of Actions


-- | This is the parse result for how to run JS functions so that we only
-- | read the total script once
-- | NOTE: that the only functions that are pointed to are unable to be determined
-- | as expressions in the context of the calling function since such case will
-- | just be treated as a value in this function
memoize' :: Map NamedFunc [NamedFunc]
memoize' = undefined

-- | Where a JSFunction takes n args
compileJSStringForExec :: Name -> Map NamedFunc [Name] -> JSFunction
compileJSString funcName ast =
  let out = Map.lookup funcName ast
  in script <> fmap (flip compileJSStringForExec ast) names



mkScript :: Html -> MaybeT IO (JSString)
mkScript = do
  locals <- runScraperOnHtml (el "script" [])
  let
    remotes = lensOfSrc locals -- traverse and grab any existing script tags src attribute
  totalRemote <- fmap (foldr <> "") $ mapM getHtml' remotes
  return $ mconcat $ fmap innerText' locals <> (totalRemote :[])

-- Ideally, such names could be looked up lazily
retrieveFunction :: JSString -> JSFunction
retrieveFunction jsString = do
  (self, called) <- runScraperOn (jsFunctionPattern) jsString
  fix $ mapM (flip runScraperOn jsString) called

runScraperOn = runScraperOnHtml --



instance Monad Elem' where
  Elem' e a t m >>= f = undefined
  -- the a in Elem' a is our value
  -- This allows a user


-- OR something like this that allows for innerText to be consumed as state
instance MonadState Elem' where

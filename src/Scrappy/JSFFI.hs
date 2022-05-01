{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-} 

module Scrappy.JSFFI where

-- foreign import javascript unsafe
-- "1+1" 

-- foreign import javascript unsafe "$1 + $2" add :: Int -> Int -> Int



tuple = do
  open
   (noneOf [',', ')'])


jsInnerParser :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m Char -> ParsecT s u m String
jsInnerParser open close = do
  (src, bracket) <- manyTill_ (try (bracketTree open close) <|> ((:[]) <$> anyChar) close
  pure (mconcat src <> (bracket :[]))

bracketTree :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m Char -> ParsecT s u m String
bracketTree open close = do
  open
  inner <- jsInnerParser open close
  pure inner

WhileLoop + DoWhileLoop
ForLoop
IfStatement
SwitchStatement
Function
ObjectDeclaration


Iterator
VarDeclaration
JSExpression -- including Object instantiation
DomRef -- ~JSExpression

type RawJS = String 


Execution model could be that I filter out things that can be lazy and only run
statements that would potentially cause an effect

filtered       :    Function, Object, 
executed direct:    WhileLoop, ForLoop, Exec, CaseStatement



data JSStatement = WhileLoop [Dependency] RawJS
     		 | ForLoop [Dependency] RawJS -- note that multiple 'lets' could be used
		 | CaseStatement (Map Condition RawJS) -- would also need ?: syntax 
		 | Function Name RawJS
		 | ObjectDeclaration Name RawJS
		 | Exec JSExpression -- in current top level as raw statement, no assignment
		 | VarAssign Name JSExpression -- this could be an empty expression 
		 | Iterator 

type Dependency = Name

-- this is derived from stream editing 
data JSExpression = JSExpr [Dependency] RawJS 

I could also use bracketTree in JSExpression

let c = { 'a' : 5 }  --> VarDecl Object
let c = 1 --> VarDecl Int


How could we handle an iterator?

I guess since we

Map Name JSValue 

data JSValue = JSValue JSStatement [Update]

data Update = Reassign JSExpression
     	    | Iterate JSStatement 

execJS :: [JSStatement]
execJS = do
  case js of
     Runnable -> do runJSWithCli =<< (substitute js) <$> get 
     NotRunnable -> put to state
     Iterator -> update state where state :: Map Name JSValue

And this is an atomic step we can take 

tuple = undefined

-- | Note that is purely done out of weird timing
-- | as long as I have the Expression as 
jsExpression :: ParsecT s u m [ExprPiece]
jsExpression = do
  manyTill piece (char '\n' <|> char ';')
  where
    piece = jsVal -- can scrape for deps
            <|> domRef 
            <|> operator  -- should include typeof
	    <|> objectInstantiation -- obj dep 
	    <|> anonFunc  
	    <|> indexedValue -- update
	    <|> functionApp -- fun dep
	    <|> varRef -- dep
	    <|> deletion -- update
	    <|> (bracketTree (char '(') (char ')'))
	    <|> wordOperators -- instanceof, in, void, typeof 
	    <|> thisOrSuperReference
	    <|> objectRef
	    <|> returnStatement

     operator = logical
              <|> bitwise
	      <|> iterational -- like ++
	      <|> math

     -- this parser must come after we've done domRef
     varRef = alpaNum >> maybeParse (char '.' >> varRef)

     jsValue = aeson with .property possibility 
	      
-- where Dependencies would come from 
jsSetValue :: ParsecT s u m ((Name, [Dependency], [ExprPiece])) 
jsSetValue = do
   manyLeftSide
   maybeParse (assignmentOperator >> expression) 

   where assignmentOperator = char '='
                            <|> (otherOperator >> (char '='))

-- with destructuring
-- var [one, two, three] = foo;

-- Also possible: constrained by must be left hand side 
-- let g = x['hey'] = f();

   -- so this can happen as long as there is no right hand side operations 

there's also an indexed assignment

let x[1] = 4 -- we can consider this an update to x



one other possible way I could handle Objects is that if i keep the object
then I can see how the object is used and if lets say the following happens:

let x = new Object()
let y = x.someMethod(1)

We scrape the inner function of .someMethod then apply it like a normal function
but we'd need to handle for some cases where the object is constructed 
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-} 

{-|
Known Exceptions:

Does not handle Object.*(someObj, js) cases (of Type Update)

|-}

module Scrappy.JSFFI where



do
  letJS name value
  withJS [name1, name2] $ \(x:x2) -> 



-- foreign import javascript unsafe
-- "1+1" 

-- foreign import javascript unsafe "$1 + $2" add :: Int -> Int -> Int

value :: ParsecT s u m JSString 
value = do
  tuple
  <|> jsBracketTree
  <|> string
  <|> array
  <|> bool
  <|> null 
  <|> number
  <|> reference

-- TODO(galen): try solving annoying Show problem by using Char vs Parser Char as args 
jsArray = bracketTree (char '[') (char ']') 

  
-- This allows me to specially get args from tuples 
tuple :: JSTuple 
tuple = do
  o <- (:[]) <$> open
  (args, end') <- manyTill_ ((tuple <|> many (noneOf [',', ')'])) <* (optional $ char ',')) end
  pure $ Tuple args (o <> (intercalate ',' args) <> end)
  
   --(noneOf [',', ')'])


jsInnerParser :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m Char -> ParsecT s u m String
jsInnerParser open close = do
  (src, bracket) <- manyTill_ (try (bracketTree open close) <|> ((:[]) <$> anyChar)) close
  pure (mconcat src <> (bracket :[]))

bracketTree :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m Char -> ParsecT s u m String
bracketTree open close = do
  open
  inner <- jsInnerParser open close
  pure inner

                    
                    

jsInnerParser :: Stream s m Char => ParsecT s u m Char -> ParsecT s u m Char -> ParsecT s u m String
jsInnerParser open close = do
  (src, bracket) <- manyTill_ (try (bracketTree open close) <|> ((:[]) <$> anyChar)) close
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

execJS :: [JSStatement] -> MonadJS () 
execJS = do
  case js of
     Runnable -> do runJSWithCli =<< (substitute js) <$> get 
     NotRunnable -> put to state
     Iterator -> update state where state :: Map Name JSValue
     LocationChange ->
       -- this JS Context is now Void except Window object 

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

-- | We should check that this isnt a function or class before we parse it 
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


-- note: we will need a SuperClass parser (a Extends b)

let x = 1

let ob = new SomeObject(x)

ob.func(23)

let y = ob.result



class SomeObject {
    constructor {}

    a = 

}



let x = new Obj() -- this calls constructor

let g = x.r(3)
let h = x.r(3)

g === h ? 

x.addIn(3)
-- this is an expression that would force all updates on x from the time of instantiation
  -- so it may or may not affect some demanded 




jsFunction = undefined
jsClass = undefined
jsSubClass = undefined
jsWhileLoop = undefined
jsForLoop = undefined
jsIfStatement = undefined
jsSwitchStatement = undefined 


-- for DOM updates, how I can handle related memory of the DOM that would depend on Update OR Deletion
-- like if a parent is deleted , the child should not be found

 -- Remedy:
  streamEdit the DOM with all updates to TimePoint t
   -> when a DOM element is asked for


if we actually update the State of JSMonad then we have sharing

let y = x + 1 -- this would force computation of y and x, if y is ever evaluated in a dependency


x += 1

let z = x + 1


If I really was motivated to make this happen I could track when the statement exists
including updates (would also have a tracked step Num)

It wouldnt matter that the Dependency itself had an update after the time that this Dependency was used


let x = 1
let y = x
x += 1


y == 1

so: if we have X stored as

  "x" : (origin, [update1, update2, update3])

  and it is forced to evaluate:

  in the process we find x == 6

  so:
    "x" : (6, [])

  and continues like a newly instantiated variable would



let x = 1
let y = x -- x == 1 (for all eternity) 
x += 1  -- x == 2 
let z = x

x += y 


So:

  "x" : ((1, initialValueX), [(3, "x+=1"), ...])

   AND

   "y" : ((2, initialValueY), [])

   a var will take from its dependencies up until when it was instantiated -- But shit this doesnt work for Objects
   (2 < 3) so it doesnt run x+=1 to get its Deps value 


   We can optimize for sharing by replacing the *how* to get to State_i with the value of it

   so X would be replaced with

   "x" : ((1, NF 1), [(3, NF 2), (16, WHNF "x+=y")]



Map Ref JSValue

data Ref = VarRef Name
         | FuncRef Name
         | ClassRef Name [Dependencies] deriving Ord -- this is the Key for the JSAST






-- Need to redo func to process conditionals

-- Need to redo execFuncEnv
  -> This will call some statements with all global variables and the given required vars



  
  


We could function-ize loops by scraping the affected variables in the loop statement and just console.logging the
value of these referenced variables 

switch ~~ If where (==Case)
and case is oneof JSValues

So in implementation switch will just parse to a conditional with (==Case) set


SetVar is significant in implementation because it affects global state

all SetVar is tho is (Name, JSExpression)

SetVar has a specific form but can be any JS and so we'd need to handle the following too:

   let f = function fAnonymousUnreffable(x,y,z) { ... }  ~~~=~~~ function f(x,y,z) 

   -- same with class


An easy way I could handle Variables when running them (before laziness is implemented) is just run in the node
cli then grab the var being set via console.log

WE could differentiate between instantiation logic and update logic

where update logic is lazy and compressable / WHNF and instantiaton logic is as follows:

if object
then do nothing yet
else jsExpression -> eval jsExpression so that var x = 1 or similar


   JSInstance -> (x, Value 1) | (x, NewObject ClassName) 

   JSValue = JSI JSInstance [Update]

    


when

obj.something(...)

this can both return a value and update internal state

so the following is valid JS

let x = someObj.result()

SO this statement should qualify as an update and a SetVar where x == all updates to now

and the update is stored as:

someObj.result() -- so result is thrown out but update would persist in this evaluation


and ofc type Update = JSStatment


-- Functions

Will be of the shape:

Function Name [ArgNames] RawJS

1) Inject LocalVars with ArgNames --labelled to not be console.logged
2) Go until I run out of statements || case(ReturnStatement) 

-- A function has the ability to affect global state
-- we determine this by the variables updated and declared (varname = value) 
runFunction :: JSAST -> JSASTLocal -> MonadJS (Maybe a)

runFunction a b = runMonadJS (a <> b)


A shitty way I could implement return functionality is when in a function use the normal eval func I make
but the eval func:

scrappyEvalJS = do
  ...
  case statement of
    ...
    Return _ -> error "invalid return"


and so runFunction:

  case statement of
    Return a -> pure . Just $ a
    s -> scrappyEvalJS s



For objects even tho the thing itself is complicated we only need to know dependencies when evaling
a statement

So if we can translate a dependency on a class to a function statement (which has been preceded with the constructor
being evaluated into a local JSASTLocal)

class Thing { 
 constructor() { A }
 function() { B }
 }

effectively becomes:

  statements = A `then` B :: [JSStatement] -> MonadJS (Maybe a)

  and this.* will be the names of the constructor memory

  and I need to do this in order to run statement by statement 


WE also need to have a special way to parse function apps and object insts

new Obj(dep1, dep2, dep3..)

f(dep1, dep2, dep3)


The difference between an object and a normal variable is that the initial value is always a real
expression (1 may be an expression that simply evals to 1) whereas for an object it is the result of
an AST that has been affected by the constructor

type Object = JSAST

So while a function also has a local JSAST, an Object persists this across time

so I could store as:


  let x = new Obj()

  "x" : (resultConstruct :: Object JSAST, [] :: [Update])  -- in a Map Ref ((Object | JSValue),  [Update])

data SomeValue = JSValue RawJS
               | Object ([Args] -> Map Name SomeValue) RawJS  

data JSAST = JSAST { varRefs :: Map Name (SomeValue, [Update]) -- includes object instances 
                   , functionRefs :: Map Name RawJS
                   , objectRefs :: Map Name Constructor RawJS
                   }


type Update = SomeValue -> SomeValue 

State = Object JSAST | JSValue RawJS




-- calling a JS function for an expression

a <*> b <*> (fromMaybe "undefined" <$> runMonadJS name (localAst <> globalAst)) <*> ... <*>  

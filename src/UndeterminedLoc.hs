module UndeterminedLoc where

-- I legit just havent figured out where to place these random functions in my library is all

  
getHtml :: Manager -> Link -> IO TextHtml
getHtml = undefined
 -- just get barebones html response for if you want to use the text many many times
  -- inside of a single execState loop-case


-- | Not super useful, might as well use findNaive if you know how, in order to avoid a parser iteration
getHtmlHeadtag :: Manager -> Link -> IO TextHtml
getHtmlHeadtag = undefined
-- | Not super useful, might as well use findNaive if you know how, in order to avoid a parser iteration
getHtmlBodytag :: Manager -> Link -> IO TextHtml
getHtmlBodytag = undefined


-- scrapeUrlWith ~= parse parser "" (<-getHtml)



-- maybe I should allow to pass a general predicate that checks the parse





--idea
-- data Scope a = Scope (a -> a)





-- Just renamed to be more general to source
-- "find" is meant to be a parsing language construct
-- like many except there can be < whatever > in between matches


baseParser :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Either ScrapeFail a)
baseParser parser = fmap Right parser

givesNothing :: Stream s m Char => ParsecT s u m (Either ScrapeFail a) 
givesNothing = Left NonMatch <$ anyChar

endStream :: (Stream s m t, Show t) => ParsecT s u m (Either ScrapeFail b)
endStream = eof >> (return $ Left Eof)

-- Right a
-- Left Eof
-- Left NonMatch

getOnlyBody = undefined -- gets html body tag
getOnlyHead = undefined -- gets only head ,, such that getOnlyHead <> getOnlyBody == fullHtml


nonInfiniteParser = undefined
-- not actually infinite but used for inner parsers and ensures that if their parser consumes
-- all input in cases where this is not possible since by nature of the format, it needs to end on
-- some particular end wrapper such as an end tag






--Equivalent functionality ; bottom one is easier ; top may not be necessary



-- | Note: Could even have framework that allows user to create a findAllMany like func on
-- | An arbitrary datatype

--eg data Example = ExampleOpt String | ExOpt (Int, Int) | ... | to nth option

-- instance HtmlParsable Example where ...


-- | Designed to be eventually split up into NLP-like module ; text/paragraph-based and HTML structure

-- | Also depends how domain-specific these things are 

-- | Note: we could introduce type safety by enforcing somehow the rule: (noneOf (endTag))

--even shittily:

-- accept datatype:

-- data Apply a = Apply { yourFunc :: a
--                      , cuff :: ParsecT s u m Error
--                      }

-- Idk or just fix elemParser and treeElemParser to cuff the matcher internally ??

-- eg: allow spaces, number and letter to get at inside text of elements

-- | Find table and maybe return as table [[a]]
findTableH :: ParsecT s u m (TreeHTML a)
findTableH = undefined



-- unorderedTwoItems :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m c
-- unorderedTwoItems a b = (a >> b) <|> (b >> a)

data OfThree a b c = First a | Second b | Third c 

oneOf3Parser a b c = (First <$> a) <|> (Second <$> b) <|> (Third <$> c)


 
findAllTill = undefined
-- findAllTill = do
--   let f :: ParsecT s u m [Either a b]
--       f = manyTill (eitherP anyChar (opt1 <|> opt2 <|>...)) desiredClosingTag

--       f' = do
--         x <- f
--         return $ catEithers x
--     in f'
    
        
findAllBetween = undefined



-- | Use with constructed for parsing datatype 
buildSequentialElemsParser :: ParsecT s u m [a]
buildSequentialElemsParser = undefined -- | to be applied to inner text of listlike elem


findOnChangeInput :: ParsecT s u m (Elem' a)
findOnChangeInput = undefined
-- eg : <select id="s-lg-sel-subjects" name="s-lg-sel-subjects" class="form-control" data-placeholder="All Subjects" onchange="springSpace.publicObj.filterAzBySubject(jQuery(this).val(), 3848);">


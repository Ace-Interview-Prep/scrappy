module ChainHTML where

-- functions for chaining free-range html patterns based on the previous
-- patterns to allow for maximum flexibility 




clean :: String -> String
clean = undefined -- drop if == ( \n | \" | '\\' )


-- same site is guranteed
allLinks :: ParsecT String () Identity [String] 
allLinks = do
  x <- findNaive hrefParser 
  return $ case x of
    Just (x':xs') -> catMaybes $ fmap (maybeUsefulUrl baseUrl) (x':xs')
    Just [] -> []
    Nothing -> [] 





-- I could always make this generalized to a stream by making
  -- Stream s => .. data Elem2' = Elem2' s  ...
contains :: ParsecT s u m (Elem' a) -> ParsecT String () Identity b -> ParsecT s u m b
contains a b = do
  x <- a

  let
    ridNL p = (many (char ' ' <|> char '\n')) >> p 
  
  -- need to skip 
  case parse (ridNL b) "" (innerText' x) of
    Right match -> return match
    Left err -> parserFail (show err)

-- finds multiple matches anywhere inside the passed elem
contains' :: ShowHTML a =>
             ParsecT s u m (Elem' a) 
          -> ParsecT String () Identity b
          -> ParsecT s u m (Maybe [b])
contains' a b = do
  x <- a
  case parse (findNaive b) "" (innerText' x) of
    Right match -> return match
    Left err -> parserFail (show err)




sequenceHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
sequenceHtml p1 p2 = do
  x <- p1
  _ <- many (char ' ' <|> char '\n')
  y <- p2
  return (x, y)

sequenceHtml_ :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b
sequenceHtml_ p1 p2 = do
  _ <- p1
  _ <- many (char ' ' <|> char '\n')
  p2

(</>>) :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b 
(</>>) = sequenceHtml_

(</>>=) :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
(</>>=) = sequenceHtml
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

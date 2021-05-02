{-# LANGUAGE FlexibleContexts #-}

module Elem.ITextElemParser where

import Elem.ElemHeadParse (parseOpeningTag)
import Elem.Types (HTMLMatcher(..), endTag, selfClosingTextful)

import Text.Megaparsec as MParsec (some)
import Text.Parsec (ParsecT, Stream, string, (<|>), anyChar, char, optional, try, manyTill)




-- justPlaintext :: Int -> ParsecT s u m [String]
-- justPlaintext atLeast = 
--   fmap (filter (\x -> length x > atLeast)) {--} (find $ many (letter <|> number <|> char ' ' <|> char '.'))
  



-- | Note: will need more complex accumulator for case where an elem has two distinct text segements broken up
-- | by an element, (rare case)
onlyPlainText :: Stream s m Char => ParsecT s u m [String]
onlyPlainText = fmap (\(ACT strings) -> strings) specialElemParser 
  where
    specialElemParser :: Stream s m Char => ParsecT s u m (AccumITextElem String)
    specialElemParser = do
      (elem', attrs') <- parseOpeningTag Nothing []  
      (localText, inTex) <- fmap (foldr textOnlyFoldr mempty)
                                $ (try (string "/>") >> return [])
                                <|> (try $ innerElemParser' elem')
                                <|> (selfClosingTextful Nothing) -- did not have an easily associated end tag
      return $ ACT (localText : inTex) 
      
      where innerElemParser' eTag = --htmlGenParser with specialElemParser 
              char '>'
              >> manyTill (Element <$> (try specialElemParser) 
                           <|> ((IText . (:[])) <$> anyChar)  ) (endTag eTag)

            -- selfClosingTextful = manyTill (IText . (:[]) <$> anyChar) endTagg
            -- endTagg = (try (char '<'
                            -- >> (optional (char '/'))
                            -- >> MParsec.some anyChar
                            -- >> (string " " <|> string ">")))

-- Not for getting matches 
data AccumITextElem a = ACT [String]

textOnlyFoldr :: HTMLMatcher AccumITextElem String -> (String, [String]) -> (String, [String]) 
textOnlyFoldr htmlM (itextAccum, fromElemAccum) = case htmlM of 
  IText str -> 
    (itextAccum <> str, fromElemAccum) 
  Element (ACT strList) ->
    (itextAccum, fromElemAccum <> strList)
  -- should never fire
  Match mat ->
    (itextAccum <> mat, fromElemAccum)

-- Should consider using the following data structure




-- textOnlyFoldr :: [HTMLMatcher e a] -> [String] 
-- textOnlyFoldr htmlMs = fmap f htmlMs -- . filter (\x -> case x of { IText x -> True; _ -> False }) htmlMs
--       where
--         f htmlM = case htmlM of 
--                     Match str -> str
--                     IText str -> str
--                     Element e  -> innerText' e
                      

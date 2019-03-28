module Kail
    ( parse
    )
where

import           Text.ParserCombinators.ReadP
import           Control.Applicative            ( (<|>) )

data Line = Line (Maybe Char) Int String | Empty deriving Show

parse :: String -> Bool -> IO ()
parse contents bail = print lines
    where lines = takeLast $ readP_to_S (many (line <|> empty)) contents

takeLast :: [(a, String)] -> Maybe a
takeLast s = if remainder == "" then Just result else Nothing
    where (result, remainder) = last s

line :: ReadP Line
line = do
    indent  <- munch (== ' ')
    op      <- maybs $ satisfy isOperator
    content <- munch1 (\ch -> ch /= '\r' && ch /= '\n')
    newline
    return $ Line op (length indent) content

empty :: ReadP Line
empty = newline >> return Empty

maybs :: ReadP a -> ReadP (Maybe a)
maybs p = (Just <$> p) <++ return Nothing

isOperator :: Char -> Bool
isOperator ch = ch == ':' || ch == '#' || ch == '>'

newline :: ReadP ()
newline = do
    string "\r\n" <++ string "\r" <|> string "\n"
    return ()

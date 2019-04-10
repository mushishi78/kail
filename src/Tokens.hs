{-# OPTIONS_GHC
    -Wall
    -Wcompat
    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Widentities
    -Wredundant-constraints
    -Wmonomorphism-restriction
    -Wmissing-home-modules
#-}

module Tokens
    ( tokenize
    , Token(..)
    )
where

import qualified Data.Char                     as Char
import           Text.ParserCombinators.ReadP
import           Control.Applicative            ( (<|>) )
import           Control.Monad

data Token =
    ImportToken String
    | CommentToken String
    | KeyToken String
    | ValueToken String
    | SpaceToken String
    | NewlineToken
    | IllegalToken String deriving Show

tokenize :: String -> Maybe [Token]
tokenize contents = takeLast $ readP_to_S (many (token <++ illegal)) contents
  where
    token =
        importToken
            <|> commentToken
            <|> valueToken
            <|> keyToken
            <|> spaceToken
            <|> newline
    importToken  = startsWith ':' >> ImportToken <$> munch isNotNewLine
    commentToken = startsWith '#' >> CommentToken <$> munch isNotNewLine
    valueToken   = startsWith '>' >> ValueToken <$> munch isNotNewLine
    keyToken     = KeyToken <$> munch1 Char.isAlpha
    spaceToken   = SpaceToken <$> munch1 (== ' ')
    newline =
        (string "\r\n" <++ string "\r" <|> string "\n") >> return NewlineToken
    illegal = IllegalToken <$> munch1 isNotNewLine

takeLast :: [(a, String)] -> Maybe a
takeLast [] = Nothing
takeLast s = case last s of
    (result, "") -> Just result
    _ -> Nothing

startsWith :: Char -> ReadP ()
startsWith ch = do
    rest <- look
    case rest of
        (c : _) -> unless (c == ch) pfail
        []      -> pfail

isNotNewLine :: Char -> Bool
isNotNewLine ch = ch /= '\r' && ch /= '\n'


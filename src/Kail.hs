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

module Kail
    ( parse
    )
where

import           Text.ParserCombinators.ReadP
import           Control.Applicative            ( (<|>) )
import           Prelude                 hiding ( lines
                                                , error
                                                )

data Line = Line (Maybe Char) Int String
    | InvalidLine String
    | WhitespaceLine String
data File = File (Maybe String) [Key] [ParseError] [ParseWarning] deriving Show
data Key = Key String Int Child deriving Show
data Child = Null | Keys [Key] | Value Int Int String deriving Show
data ParseError = UnableToParse
    | NoSchema
    | UnparsedLine Int String
    | UnrecognizedOperator Int Char
    | NoContent Int deriving Show
data ParseWarning = NoSpaceAfterOperator Int
    | TrailingWhitespace Int deriving Show

parse :: String -> Bool -> IO ()
parse contents _bail = do
    print lines
    print file
  where
    lines = takeLast $ readP_to_S
        (many ((operatorLine <|> whitespaceLine) <++ keyLine <++ invalidLine))
        contents
    file = case lines of
        (Just ls) -> createFile ls
        Nothing   -> File Nothing [] [UnableToParse] []

takeLast :: [(a, String)] -> Maybe a
takeLast s = if remainder == "" then Just result else Nothing
    where (result, remainder) = last s

--
-- Parsing lines

instance Show Line where
    show (Line (Just op) indent content) =
        "OperatorLine "
            ++ show indent
            ++ " "
            ++ [op]
            ++ " '"
            ++ content
            ++ "'\n"
    show (Line Nothing indent content) =
        "KeyLine " ++ show indent ++ " '" ++ content ++ "'\n"
    show (WhitespaceLine _      ) = "WhitespaceLine\n"
    show (InvalidLine    content) = "InvalidLine" ++ " '" ++ content ++ "'\n"

operatorLine :: ReadP Line
operatorLine = do
    indent  <- munch (== ' ')
    op      <- satisfy (\ch -> ch == ':' || ch == '#' || ch == '>')
    content <- munch (\ch -> ch /= '\r' && ch /= '\n')
    newline
    return $ Line (Just op) (length indent) content

keyLine :: ReadP Line
keyLine = do
    indent  <- munch (== ' ')
    content <- munch1 (\ch -> ch /= '\r' && ch /= '\n')
    newline
    return $ Line Nothing (length indent) content

whitespaceLine :: ReadP Line
whitespaceLine = do
    whitespace <- munch (== ' ')
    newline
    return $ WhitespaceLine whitespace

invalidLine :: ReadP Line
invalidLine = do
    content <- munch1 (\ch -> ch /= '\r' && ch /= '\n')
    newline
    return $ InvalidLine content

newline :: ReadP ()
newline = do
    _ <- string "\r\n" <++ string "\r" <|> string "\n"
    return ()

--
-- Creating a "File"

createFile :: [Line] -> File
createFile lines =
    hasSchema $ foldr processLine (File Nothing [] [] []) (zip lines [0 ..])

processLine :: (Line, Int) -> (File -> File)
processLine (ln, i) = case ln of
    WhitespaceLine content  -> noTrailingSpace i content
    InvalidLine    content  -> addError (UnparsedLine i content)
    Line ch _indent content -> case ch of
        (Just ':') ->
            hasContent i content
                . spaceAfterOperator i content
                . noTrailingSpace i content
        (Just '#') -> spaceAfterOperator i content
        (Just '>') -> spaceAfterOperator i content
        (Just op ) -> addError (UnrecognizedOperator i op)
        Nothing    -> id

hasSchema :: File -> File
hasSchema f = case f of
    (File Nothing _ _ _) -> addError NoSchema f
    _                    -> f

hasContent :: Int -> String -> File -> File
hasContent i content f = if content /= [] then f else addError (NoContent i) f

spaceAfterOperator :: Int -> String -> File -> File
spaceAfterOperator i content f = case headMay content of
    Just ' ' -> f
    Just _   -> addWarning (NoSpaceAfterOperator i) f
    Nothing  -> f

noTrailingSpace :: Int -> String -> File -> File
noTrailingSpace i content f = case lastMay content of
    Just ' ' -> addWarning (TrailingWhitespace i) f
    _        -> f

addError :: ParseError -> File -> File
addError error (File schema keys errors warnings) =
    File schema keys (errors ++ [error]) warnings

addWarning :: ParseWarning -> File -> File
addWarning warning (File schema keys errors warnings) =
    File schema keys errors (warnings ++ [warning])

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay ls = Just $ last ls

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay ls = Just $ head ls

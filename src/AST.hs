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

module AST
    ( createAST
    , emptyAST
    , AST(..)
    )
where

import qualified Data.Char                     as Char
import           Tokens
import           Prelude                 hiding ( error
                                                )

data AST = AST
    { lineCount :: Int
    , schema :: Maybe String
    , keys :: [Key]
    , errors :: [ParseError]
    , warnings :: [ParseWarning]
    } deriving Show

data Key = Key String Child deriving Show
data Child = Null | Keys [Key] | Value String deriving Show

data ParseError = NoSchema
                | UnhandledTokenSequence
                | AlreadyHasSchema Int
                | SchemaAfterKeys Int
                | LineWithMultipleKeys Int
                | LineWithKeyAndValue Int
                | IndentedTooFar Int
                | NoParentKey Int
                | KeyAddedToValue Int
                | ValueMustBeForKey Int
                | NoContent Int deriving Show

data ParseWarning = NoSpaceAfterOperator Int
                  | TrailingWhitespace Int
                  | IndentUnaligned Int deriving Show

emptyAST :: AST
emptyAST = AST { lineCount     = 1
               , schema   = Nothing
               , keys     = []
               , errors   = []
               , warnings = []
               }

createAST :: [Token] -> AST
createAST tokens = hasSchema . reverseKs $ assemble tokens emptyAST
    where
        hasSchema ast = case schema ast of
            Nothing -> addError NoSchema ast
            _       -> ast
        reverseKs ast = ast { keys = (reverseKeys (keys ast)) }

assemble :: [Token] -> AST -> AST
assemble (ImportToken content:tokens) = assemble tokens . importStatement content
assemble (KeyToken content:tokens) =
    assemble (findNewLine tokens) . key tokens "" content
assemble (SpaceToken indent:KeyToken content:tokens) =
    assemble (findNewLine tokens) . key tokens indent content
assemble (ValueToken content:tokens) = assemble tokens . value "" content
assemble (SpaceToken indent:ValueToken content:tokens) = assemble tokens . value indent content
assemble (SpaceToken _:tokens) = assemble tokens
assemble (CommentToken content:tokens) = assemble tokens . comment content
assemble (NewlineToken:tokens) = assemble tokens . incrementLineCount
assemble [] = id
assemble _ = addError UnhandledTokenSequence

importStatement :: String -> AST -> AST
importStatement (':':content) =
    addSchema content .
    beforeKeys .
    firstSchema .
    noTrailingSpace content .
    spaceAfterOperator content .
    hasContent content
    where
        firstSchema ast = case schema ast of
            (Just _) -> addLineError AlreadyHasSchema ast
            Nothing -> ast
        beforeKeys ast = case keys ast of
            (_:_) -> addLineError SchemaAfterKeys ast
            [] -> ast
        addSchema content ast = ast { schema = Just (trim content) }

key :: [Token] -> String -> String -> AST -> AST
key nextTokens indent content =
    checkNextTokens nextTokens .
    indentAlignment indent .
    addKey
    where
        checkNextTokens (SpaceToken _:KeyToken _:_) = addLineError LineWithMultipleKeys
        checkNextTokens (SpaceToken _:ValueToken _:_) = addLineError LineWithKeyAndValue
        checkNextTokens _ = id
        addKey ast = case insertKey (indentCount indent) (Key content Null) (keys ast) of
            Left err -> addLineError err ast
            Right ks -> ast { keys = ks }

comment :: String -> AST -> AST
comment ('#':content) = noTrailingSpace content . spaceAfterOperator content

value :: String -> String -> AST -> AST
value indent ('>':content) =
    addValue .
    indentAlignment indent .
    noTrailingSpace content .
    spaceAfterOperator content
    where
        value = Value $ trimFirstSpace content
        addValue ast = case insertValue (indentCount indent) value (keys ast) of
            Left err -> addLineError err ast
            Right ks -> ast { keys = ks }

findNewLine :: [Token] -> [Token]
findNewLine (NewlineToken:tokens) = (NewlineToken:tokens)
findNewLine (_:tokens) = findNewLine tokens
findNewLine [] = []

indentAlignment :: String -> AST -> AST
indentAlignment indent =
    if mod (length indent) 4 == 0
        then id
        else addLineWarning IndentUnaligned

indentCount :: String -> Int
indentCount indent = div (length indent) 4

insertKey :: Int -> Key -> [Key] -> Either (Int -> ParseError) [Key]
insertKey 0 k ks = Right $ k : ks
insertKey 1 k (Key label Null:ks) = Right $ Key label (Keys [k]) : ks
insertKey _ _ (Key _ Null:_) = Left IndentedTooFar
insertKey _ _ [] = Left NoParentKey
insertKey _ _ (Key _ (Value _):_) = Left KeyAddedToValue
insertKey i k (Key label (Keys sub):ks) = case insertKey (i - 1) k sub of
    Left err -> Left err
    Right sub -> Right $ Key label (Keys sub) : ks

reverseKeys :: [Key] -> [Key]
reverseKeys = reverse . fmap f
    where
        f (Key label child) = case child of
            Keys ks -> Key label $ Keys $ reverseKeys ks
            _ -> Key label child

insertValue :: Int -> Child -> [Key] -> Either (Int -> ParseError) [Key]
insertValue 0 _ ks = Left ValueMustBeForKey
insertValue 1 value (Key label Null:ks) = Right $ Key label value : ks
insertValue 1 (Value next) (Key label (Value previous):ks) =
    Right $ Key label (Value (previous ++ "\n" ++ next)) : ks
insertValue _ _ (Key _ Null:_) = Left IndentedTooFar
insertValue _ _ [] = Left NoParentKey
insertValue _ _ (Key _ (Value _):_) = Left IndentedTooFar
insertValue i value (Key label (Keys sub):ks) = case insertValue (i - 1) value sub of
    Left err -> Left err
    Right sub -> Right $ Key label (Keys sub) : ks

incrementLineCount :: AST -> AST
incrementLineCount ast = ast { lineCount = lineCount ast + 1 }

hasContent :: String -> AST -> AST
hasContent content =
    if not (all Char.isSpace content)
        then id
        else addLineError NoContent

spaceAfterOperator :: String -> AST -> AST
spaceAfterOperator content = case headMay content of
    Just ' ' -> id
    Just _   -> addLineWarning NoSpaceAfterOperator
    Nothing  -> id

noTrailingSpace :: String -> AST -> AST
noTrailingSpace content = case lastMay content of
    Just ' ' -> addLineWarning TrailingWhitespace
    _        -> id

addError :: ParseError -> AST -> AST
addError error ast = ast { errors = errors ast ++ [error] }

addLineError :: (Int -> ParseError) -> AST -> AST
addLineError constructor ast = addError (constructor (lineCount ast)) ast

addWarning :: ParseWarning -> AST -> AST
addWarning warning ast = ast { warnings = warnings ast ++ [warning] }

addLineWarning :: (Int -> ParseWarning) -> AST -> AST
addLineWarning constructor ast = addWarning (constructor (lineCount ast)) ast

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay ls = Just $ last ls

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay ls = Just $ head ls

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile Char.isSpace

trimFirstSpace :: String -> String
trimFirstSpace (' ':s) = s
trimFirstSpace s = s


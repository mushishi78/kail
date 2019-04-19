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
    ( parseDataFile
    , parseSchemaFile
    , tokenize
    , createAST
    , AST (..)
    , Token (..)
    , Schema (..)
    )
where

import           Tokens
import           AST
import           Schema

parseDataFile :: String -> Maybe AST
parseDataFile contents = createAST <$> tokenize contents

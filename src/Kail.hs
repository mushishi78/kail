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
    , tokenize
    , createAST
    )
where

import           Tokens
import           AST

parse :: String -> Maybe AST
parse contents = createAST <$> tokenize contents



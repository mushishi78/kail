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

module Schema
    ( parseSchemaFile
    , Schema(..)
    )
where

import Text.Megaparsec
import Data.Text
import qualified Data.Text.IO as TIO

type Parser a = Parsec SchemaError Text a
type Path = [String]

data Schema = Schema [Children] deriving Show
data Children = Children Path [Path] deriving Show

data SchemaError = ParseFailure deriving Show

parseSchemaFile :: Text -> IO ()
parseSchemaFile text = do
    TIO.putStrLn text


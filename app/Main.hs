{- COMPILE_FLAGS -O2 -threaded -rtsopts -eventlog -}

{-# OPTIONS_GHC
    -Werror
    -Wall
    -Wcompat
    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Widentities
    -Wredundant-constraints
    -Wmonomorphism-restriction
    -Wmissing-home-modules
#-}

{-# LANGUAGE
    OverloadedStrings,
    ScopedTypeVariables,
    QuasiQuotes,
    LambdaCase
#-}

module Main where

import qualified Kail
import Options.Applicative

data Command
    = Validate ValidateOptions
    | Naked NakedOptions

data ValidateOptions = ValidateOptions {
    file :: Maybe String,
    bail :: Bool
}

data NakedOptions = NakedOptions {
    version :: Bool
}

main :: IO ()
main = execParser (info parser mempty) >>= run

parser :: Parser Command
parser =
    subparser (command "validate" (info validateParser (progDesc "Validate a kail file")))
    <|> nakedParser
  where
    validateParser = Validate <$> (ValidateOptions <$> fileOption <*> bailOption) <**> helper
    nakedParser = Naked <$> (NakedOptions <$> versionOption) <**> helper

    fileOption :: Parser (Maybe String)
    fileOption = optional $ strOption $
        long "file" <> short 'f' <> metavar "FILE" <> help "FILE to be validated"
    bailOption = switch $
        long "bail" <> short 'b' <> help "Stop when the first error is found"
    versionOption = switch $
        long "version" <> short 'v' <> help "Display the version of kail"

run :: Command -> IO ()
run (Validate opts) = do
    contents <- case file opts of
        Just f -> readFile f
        Nothing -> getContents
    Kail.parse contents (bail opts)
run (Naked opts) =
    if version opts then
        putStrLn "1.0.0"
    else
        putStrLn "Naked!"
            
            
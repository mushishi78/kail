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
    ScopedTypeVariables
#-}

module Main where

import qualified Kail
import           Options.Applicative
import Filesystem.Path.CurrentOS
import System.Directory

data Command
    = Validate ValidateOptions
    | Naked NakedOptions

data ValidateOptions = ValidateOptions {
    file :: Maybe String
}

newtype NakedOptions = NakedOptions {
    version :: Bool
}

main :: IO ()
main = execParser (info parser mempty) >>= run

parser :: Parser Command
parser =
    subparser
            (command "validate"
                     (info validateParser (progDesc "Validate a kail file"))
            )
        <|> nakedParser
  where
    validateParser =
        Validate <$> (ValidateOptions <$> fileOption) <**> helper
    nakedParser = Naked <$> (NakedOptions <$> versionOption) <**> helper

    fileOption :: Parser (Maybe String)
    fileOption =
        optional
            $  strOption
            $  long "file"
            <> short 'f'
            <> metavar "FILE"
            <> help "FILE to be validated"
    versionOption = switch $ long "version" <> short 'v' <> help
        "Display the version of kail"

run :: Command -> IO ()
run (Validate opts) = do
    contents <- case file opts of
        Just f  -> readFile f
        Nothing -> getContents
    cwd <- case file opts of
        Just f  -> return $ parent (decodeString f)
        Nothing -> decodeString <$> getCurrentDirectory
    case Kail.parse contents of
        Nothing -> putStrLn "Parse failure"
        (Just ast) -> case Kail.errors ast of
            (e:errs) -> print (e:errs)
            [] -> case Kail.schema ast of
                Nothing -> return ()
                (Just path) -> do
                    schemaContents <- readFile $ encodeString $ cwd <> (decodeString path)
                    print ast
                    print schemaContents
run (Naked opts) = if version opts then putStrLn "1.0.0" else putStrLn "Naked!"

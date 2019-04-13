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
import qualified Filesystem.Path.CurrentOS as FP
import System.Directory
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Exception
import Data.Bifunctor

data Command
    = Validate ValidateOptions
    | Naked NakedOptions

data MainError =
    DataFileReadFail |
    ParseFail |
    ParseErrors Kail.AST |
    SchemaFileReadFail

type MainT a = ExceptT MainError IO a

data ValidateOptions = ValidateOptions {
    dataFilePath :: Maybe String
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
        Validate <$> (ValidateOptions <$> dataFileOption) <**> helper
    nakedParser = Naked <$> (NakedOptions <$> versionOption) <**> helper

    dataFileOption :: Parser (Maybe String)
    dataFileOption =
        optional
            $  strOption
            $  long "file"
            <> short 'f'
            <> metavar "FILE"
            <> help "FILE to be validated"
    versionOption = switch $ long "version" <> short 'v' <> help
        "Display the version of kail"

run :: Command -> IO ()
run (Naked opts) = if version opts then putStrLn "1.0.0" else putStrLn "Naked!"
run (Validate opts) = either renderError return =<< runExceptT run'
    where
        run' :: MainT ()
        run' = do
            -- Read the kail data file either from provided file or piped in
            dataFile <- safeIO DataFileReadFail $ maybe getContents readFile $ dataFilePath opts

            -- Get the directory that the schema path will be relative to
            -- So when the path to the data file is provided, this will be that directory
            -- Otherwise the current working directory is used
            dir <- case dataFilePath opts of
                (Just f) -> ExceptT $ return $ Right $ FP.parent $ FP.decodeString $ f
                Nothing -> liftIO $ FP.decodeString <$> getCurrentDirectory

            -- Parse the data file into an ast
            ast <- ExceptT $ return $ maybe (Left ParseFail) Right $ Kail.parse dataFile

            -- Do not continue if there are parse errors
            _ <- ExceptT $ return $ if null (Kail.errors ast) then Right () else Left $ ParseErrors ast

            -- Read the schema file
            let schemaPath = FP.decodeString $ ignoreNothing $ Kail.schema ast
            let schemaPathFromCWD = FP.encodeString $ dir <> schemaPath
            schemaFile <- safeIO SchemaFileReadFail $ readFile schemaPathFromCWD

            liftIO $ print ast
            liftIO $ print schemaFile
            return ()

renderError :: MainError -> IO ()
renderError err = case err of
    DataFileReadFail -> putStrLn "Failed to read kail data file"
    ParseFail -> putStrLn "Failed to parse kail data file"
    (ParseErrors ast) -> do
        putStrLn $ "Parse errors: " ++ show (Kail.errors ast)
        putStrLn $ "Parse warnings: " ++ show (Kail.warnings ast)
    SchemaFileReadFail -> putStrLn "Failed to read kail schema file"

safeIO :: MainError -> IO a -> MainT a
safeIO err io = ExceptT $ liftIO $ try io >>= return . (first mapError)
    where
        mapError :: IOException -> MainError
        mapError _ = err

ignoreNothing :: Maybe a -> a
ignoreNothing (Just a) = a
ignoreNothing Nothing = undefined

module Kail
    ( parse
    ) where

import Text.ParserCombinators.ReadP

data KailFile =
    KailFile Schema deriving Show

data Schema = Schema String deriving Show

parse :: String -> Bool -> IO ()
parse contents bail = putStrLn $ show $ readP_to_S kailFile contents

kailFile :: ReadP KailFile
kailFile = do
    string "kail "
    schema <- Schema <$> munch (\c -> c /= '\n')
    return $ KailFile schema



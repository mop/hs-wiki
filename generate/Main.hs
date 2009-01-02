module Main
where

import System.Environment
import Controller
import Control.Monad (when)

lookupTable :: [(String, ([String] -> IO String))]
lookupTable = [
        ("controller", generateController)
    ]

main = do
    args <- getArgs
    when (length args < 1) (fail "1 paramter must be specified")
    res <- case lookup (head args) lookupTable of
        Just fun -> fun $ drop 1 args
        Nothing  -> return ""
    putStrLn res

module Main where

import System.Environment (getArgs)
import DFA


main :: IO ()
main = do
    args <- getArgs
    let arglength = length args
    
    if (arglength == 1) && (args !! 0 `notElem` helpCommands) then
        case toInputs $ args !! 0 of
            Nothing -> printHelp
            Just i  -> do let result = evaluate i
                              s = sum result
                          putStrLn $ (show result)
                          putStrLn $ "Sum:    " ++ (show s)
    else
        printHelp


helpCommands :: [String]
helpCommands = ["help", "--help", "-h", "--h", "/h", "/?", "/help"]

printHelp :: IO ()
printHelp = do
    putStrLn "Usage: DFA <input>"
    putStrLn "<input> should be a string of characters from \"+-ne\":"
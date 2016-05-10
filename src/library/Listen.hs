{-# LANGUAGE
    FlexibleContexts,
    ConstraintKinds,
    OverloadedStrings,
    OverloadedLists,
    RankNTypes #-}

import Migration
import System.Environment (getArgs)

main :: IO ()
main = do 
    args <- getArgs
    case length args of 1 -> let port = head args
                             in listenForComp port Nothing
                        2 -> let port = head args
                                 auth = head (tail args)
                             in listenForComp port (Just auth)
                        _ -> putStrLn "Usage: Listen [port] ([auth code])"

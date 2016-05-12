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
    case length args of 1 -> let (port:_) = args
                             in listenForComp port NoAuth
                        2 -> let (port:auth:_) = args
                             in listenForComp port (Auth auth)
                        _ -> putStrLn "Usage: Listen [port] ([auth code])"

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
    if length args == 1 then do
	    let port = head args
	    listenForComp port
    else putStrLn "Usage: Listen [port]"

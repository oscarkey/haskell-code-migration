{-# LANGUAGE
    FlexibleContexts,
    ConstraintKinds,
    OverloadedStrings #-}

import Migration

registerComp :: MigrationComp ()
registerComp = do 
    printStr "How many pupils are currently present?"
    count <- readStr
    printStr "And what is your name?"
    name <- readStr
    migrate "127.0.0.1"
    printStr $ name +++ " says that the number of pupils present was " +++ count
    return ()

main :: IO ()
main = runMigrationComp registerComp
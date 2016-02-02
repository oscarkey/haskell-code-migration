{-# LANGUAGE
    FlexibleContexts,
    ConstraintKinds,
    OverloadedStrings,
    RankNTypes #-}

import Migration

readNames :: MigrationComp (AbsList AbsString)
readNames =
    let limited 0 = return Nil 
        limited x = do
            name <- readStr
            end <- name === "end"
            if end then do
                return Nil
            else do
                rest <- limited (x-1)
                return $ acons name rest
    in limited 20

registerComp :: MigrationComp ()
registerComp = do
    printStr "Which class is this?"
    className <- readStr
    printStr "Enter the present student names: ('end' to finish)"
    names <- readNames
    migrate "127.0.0.1"
    printStr $ "The people present in class " +++ className +++ " are:"
    forEach (\name -> printStr name) names
    return ()

main :: IO ()
main = runMigrationComp registerComp
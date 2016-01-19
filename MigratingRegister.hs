{-# LANGUAGE
    FlexibleContexts,
    ConstraintKinds,
    OverloadedStrings #-}

import Migration

readNames :: MigrationComp (AbsList AbsString)
readNames =
    let limited 0 = return Nil 
        limited x = do
            name <- readStr
            rest <- limited (x-1)
            return $ acons name rest
    in limited 5

registerComp :: MigrationComp ()
registerComp = do
    printStr "Which class is this?"
    className <- readStr
    printStr "Enter the present student names: (max 5)"
    names <- readNames
    migrate "127.0.0.1"
    printStr $ "The people present in class " +++ className +++ " are:"
    -- Print the list of people present
    return ()

main :: IO ()
main = runMigrationComp registerComp
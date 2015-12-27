{-# LANGUAGE TypeFamilies,
    GADTs,
    RankNTypes,
    MultiParamTypeClasses,
    QuasiQuotes,
    FlexibleInstances,
    FlexibleContexts,
    OverlappingInstances,
    UndecidableInstances,
    ConstraintKinds #-}

import Migration

registerComp :: MigrationComp ()
registerComp = do 
    printStr "How many pupils are currently present?"
    count <- readInt
    migrate "127.0.0.1"
    printStr "The number of pupils present was:"
    printInt count
    return ()

main :: IO ()
main = runMigrationComp registerComp
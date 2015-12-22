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

registerComp :: MigrationComp
registerComp = do 
    printStr "How many pupils are currently present?"
    count <- readNum
    migrate "127.0.0.1"
    printStr "The number of pupils present was:"
    printNum count
    return 0

main :: IO Int
main = runMigrationComp registerComp
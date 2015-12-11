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
    count <- readInt
    migrate
    printStr "The number of pupils present was:"
    printStore count
    return 0

main :: IO Int
main = runMigrationComp registerComp
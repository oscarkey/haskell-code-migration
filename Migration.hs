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

import ShallowFreeHandlers
import DesugarHandlers

data CompTree a = MigrateEffect (CompTree a) | Result a deriving (Show,Read)

[operation|Migrate :: ()|]

type MigrationComp a =
    ([handles|h {Migrate}|], Num a) => Comp h a

[shallowHandler|
    ReifyComp a :: CompTree a
        handles {Migrate} where
            Return x -> Result x
            Migrate k -> MigrateEffect (reifyComp (k ()))
|]

[shallowHandler|
    forward h.
        RunMigration a :: a -> a
            handles {Migrate} where
                Return d x -> return x
                Migrate d k -> {
                    tree <- reifyComp d (k ());
                    return d
                }
|]

runCompTree :: CompTree a -> MigrationComp a
runCompTree (Result x) = return x
runCompTree (MigrateEffect comp) = do {migrate; runCompTree comp}

testComp :: MigrationComp Int
testComp = do {
    migrate;
    return 2
}

--main :: IO (CompTree Int)
--main = do 
--    return (reifyComp testComp)
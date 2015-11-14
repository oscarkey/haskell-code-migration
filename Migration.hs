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

[handler|
	ReifyComp a :: CompTree a
		handles {Migrate} where
			Return x -> Result x
			Migrate k -> reifyComp (MigrateEffect (k ()))
|]

[shallowHandler|
	RunMigration a :: a
		handles {Migrate} where
			Return x -> x
			Migrate k -> reifyComp (k ())
|]

testComp :: MigrationComp String
testComp = do {
	migrate;
	return "howdy"
}

main = do 
	runMigration testComp
	return ()
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
import Network.Simple.TCP (connect, listen, accept, HostPreference(Host))
import Network.Socket (recv, send)

portNum = "8000"

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
    RunMigration a :: IO a
        handles {Migrate} where
            Return x -> return x
            Migrate k -> do {
                sendComp $ reifyComp (k ());
                listenForComp
            }
|]

runCompTree :: CompTree a -> MigrationComp a
runCompTree (Result x) = return x
runCompTree (MigrateEffect comp) = do {migrate; runCompTree comp}

listenForComp :: IO a
listenForComp = listen (Host "127.0.0.1") portNum $ \(socket, socketAddress) -> do
    putStrLn "Listening for incoming connections..."
    accept socket $ \(socket, remoteAddress) -> do
        str <- recv socket 4096
        putStrLn "Recieved computation, running it"

sendComp :: CompTree a -> IO Int
sendComp comp = do 
    connect "127.0.0.1" portNum $ \(socket, remoteAddress) -> do
        putStrLn "Sending computation"
        send socket (show comp)

testComp :: MigrationComp Int
testComp = do {
    migrate;
    return 2
}

--main :: IO (CompTree Int)
--main = do 
--    return (reifyComp testComp)
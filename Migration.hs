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

import Handlers
import DesugarHandlers
import Network.Simple.TCP (connect, listen, accept, HostPreference(Host))
import Network.Socket (recv, send)

portNum = "8000"

data CompTree = MigrateEffect CompTree | Result Int deriving (Show,Read)

[operation|Migrate :: ()|]

type MigrationComp = ([handles|h {Migrate}|]) => Comp h Int

[handler|
    RunMigration :: Bool -> IO CompTree
        handles {Migrate} where
            Return  x reify -> return (Result x)
            Migrate k reify -> if reify then do {
                child <- k () True;
                return (MigrateEffect child)
            }
            else do {
                comp <- k () True;
                sendComp comp;
                listenForComp
            }
|]

runCompTree :: CompTree -> MigrationComp
runCompTree (Result x) = return x
runCompTree (MigrateEffect comp) = do {migrate; runCompTree comp}

listenForComp :: IO CompTree
listenForComp = listen (Host "127.0.0.1") portNum $ \(socket, socketAddress) -> do
    putStrLn "Listening for incoming connections..."
    accept socket $ \(socket, remoteAddress) -> do
        str <- recv socket 4096
        putStrLn "Recieved computation, running it"
        let comp = (read str :: CompTree)
        runMigration False $ runCompTree comp;

sendComp :: CompTree -> IO Int
sendComp comp = do 
    connect "127.0.0.1" portNum $ \(socket, remoteAddress) -> do
        putStrLn "Sending computation"
        send socket (show comp)

testComp :: MigrationComp
testComp = do {
    migrate;
--main :: IO (CompTree Int)
--main = do 
--    return (reifyComp testComp)
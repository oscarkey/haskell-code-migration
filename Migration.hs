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


-- Tree to represent computations.
data CompTree = Result Int
    | MigrateEffect CompTree 
    | PrintStrEffect String CompTree deriving (Show,Read)


-- Handlers and reification.
[operation|Migrate :: ()|]
[operation|PrintStr :: String -> ()|]

type MigrationComp = ([handles|h {Migrate, PrintStr}|]) => Comp h Int

[handler|
    ReifyComp :: CompTree
        handles {Migrate, PrintStr} where
            Return       x -> Result x
            Migrate      k -> MigrateEffect (k ())
            PrintStr str k -> PrintStrEffect str (k ())
|]


-- Networking.
listenForComp :: IO Int
listenForComp = listen (Host "127.0.0.1") portNum $ \(socket, socketAddress) -> do
    putStrLn "Listening for incoming connections..."
    accept socket $ \(socket, remoteAddress) -> do
        str <- recv socket 4096
        putStrLn "Received computation, running it"
        let comp = (read str :: CompTree)
        runCompTree comp

sendComp :: (CompTree, Store) -> IO Int
sendComp (comp, store) = do 
    connect "127.0.0.1" portNum $ \(socket, remoteAddress) -> do
        putStrLn "Sending computation"
        send socket $ show comp


runCompTree :: CompTree -> IO Int
runCompTree (Result x) = return x
runCompTree (MigrateEffect comp) = do
    sendComp (comp, [])
    listenForComp
runCompTree (PrintStrEffect str comp) = do
    putStrLn str
    runCompTree comp

runMigrationComp :: MigrationComp -> IO Int
runMigrationComp comp = runCompTree (reifyComp comp)


testComp :: MigrationComp
testComp = do {
    printStr "How many pupils are present?";
    migrate;
    printStr "Print something else";
    return 3
}

main :: IO ()
main = return ()

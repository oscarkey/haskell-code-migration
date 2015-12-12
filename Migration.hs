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

module Migration where

import Handlers
import DesugarHandlers
import Network.Simple.TCP (connect, listen, accept, HostPreference(Host), HostName)
import Network.Socket (recv, send)
import qualified Data.Map.Strict as Map

portNum = "8000"


-- Store.
type StoreIndex = Int
type Store = Map.Map StoreIndex Int

save :: Store -> StoreIndex -> Int -> Store
save store i x = Map.insert i x store

retrieve :: Store -> StoreIndex -> Int
retrieve store i = let x = Map.lookup i store in
    case x of Just v -> v
              Nothing -> 0

emptyStore :: Store
emptyStore = Map.empty


-- Tree to represent computations.
data CompTree = Result Int
    | MigrateEffect HostName CompTree 
    | PrintStrEffect String CompTree
    | PrintStoreEffect StoreIndex CompTree
    | ReadIntEffect StoreIndex CompTree deriving (Show,Read)


-- Handlers and reification.
[operation|Migrate :: HostName -> ()|]
[operation|PrintStr :: String -> ()|]
[operation|PrintStore :: StoreIndex -> ()|]
[operation|ReadInt :: StoreIndex|]

type MigrationComp = ([handles|h {Migrate, PrintStr, PrintStore, ReadInt}|]) 
                        => Comp h Int

[handler|
    ReifyComp :: StoreIndex -> CompTree
        handles {Migrate, PrintStr, PrintStore, ReadInt} where
            Return          x i -> Result x
            Migrate    host k i -> MigrateEffect host (k () i)
            PrintStr    str k i -> PrintStrEffect str (k () i)
            PrintStore  str k i -> PrintStoreEffect str (k () i)
            ReadInt         k i -> ReadIntEffect i (k i (i+1))
|]


-- Networking.
listenForComp :: IO Int
listenForComp = listen (Host "127.0.0.1") portNum $ \(socket, socketAddress) -> do
    putStrLn "Listening for incoming connections..."
    accept socket $ \(socket, remoteAddress) -> do
        str <- recv socket 4096
        putStrLn "Received computation, running it"
        let (comp, store) = (read str :: (CompTree, Store))
        runCompTree (comp, store)

sendComp :: HostName -> (CompTree, Store) -> IO Int
sendComp hostName (comp, store) = do 
    connect hostName portNum $ \(socket, remoteAddress) -> do
        putStrLn $ "Sending computation to " ++ hostName
        send socket $ show (comp, store)


runCompTree :: (CompTree, Store) -> IO Int
runCompTree (Result x, store) = return x
runCompTree (MigrateEffect host comp, store) = do
    sendComp host (comp, store)
    listenForComp
runCompTree (PrintStrEffect str comp, store) = do
    putStrLn str
    runCompTree (comp, store)
runCompTree (PrintStoreEffect i comp, store) = do
    putStrLn $ show (retrieve store i)
    runCompTree (comp, store)
runCompTree (ReadIntEffect i comp, store) = do
    line <- getLine
    let store' = save store i (read line)
    runCompTree (comp, store')

runMigrationComp :: MigrationComp -> IO Int
runMigrationComp comp = runCompTree (reifyComp 0 comp, emptyStore)

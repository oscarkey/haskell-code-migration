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
data StoreKey a = StoreKey Int deriving (Show, Read)
data StoreValue = StoreIntValue Int deriving (Show, Read)
type Store = Map.Map Int StoreValue

saveInt :: Store -> StoreKey Int -> Int -> Store
saveInt store (StoreKey k) x = Map.insert k (StoreIntValue x) store

retrieveInt :: Store -> StoreKey Int -> Int
retrieveInt store (StoreKey k) = 
    let v = Map.lookup k store in
        case v of Just u  -> case u of StoreIntValue x -> x
                  Nothing -> error "No value associated with store location"


emptyStore :: Store
emptyStore = Map.empty


-- Abstract values.
data AbsInt = IntVal Int
            | IntVar (StoreKey Int)
            | OpPlus AbsInt AbsInt
            | OpMinus AbsInt AbsInt
            | OpMult AbsInt AbsInt
            | OpSig AbsInt
    deriving (Show, Read)

instance Num AbsInt where
    (IntVal x) + (IntVal y) = IntVal (x + y)
    x + y = OpPlus x y
    (IntVal x) - (IntVal y) = IntVal (x - y)
    x - y = OpMinus x y
    (IntVal x) * (IntVal y) = IntVal (x * y)
    x * y = OpMult x y
    negate x = (IntVal 0) - x
    signum x = OpSig x
    abs x = x * (signum x)
    fromInteger x = IntVal (fromInteger x)

evalAbsInt store (IntVal    x) = x
evalAbsInt store (IntVar    k) = retrieveInt store k
evalAbsInt store (OpPlus  x y) = (evalAbsInt store x) + (evalAbsInt store y)
evalAbsInt store (OpMinus x y) = (evalAbsInt store x) - (evalAbsInt store y)
evalAbsInt store (OpMult  x y) = (evalAbsInt store x) * (evalAbsInt store y)
evalAbsInt store (OpSig     x) = signum $ evalAbsInt store x


class AbsShow a where
    ashow :: Store -> a -> String

instance AbsShow AbsInt where
    ashow store x = show $ evalAbsInt store x

instance AbsShow Int where
    ashow store x = show x


-- Tree to represent computations.
data CompTree = Result Int
    | MigrateEffect HostName CompTree 
    | PrintStrEffect String CompTree
    | PrintIntEffect AbsInt CompTree
    | ReadIntEffect (StoreKey Int) CompTree deriving (Show,Read)


-- Handlers and reification.
[operation|Migrate  :: HostName -> ()|]
[operation|PrintStr :: String -> ()|]
[operation|PrintInt :: AbsInt -> ()|]
[operation|ReadInt  :: AbsInt|]

type MigrationComp = ([handles|h {Migrate, PrintStr, PrintInt, ReadInt}|]) 
                        => Comp h Int

[handler|
    ReifyComp :: Int -> CompTree
        handles {Migrate, PrintStr, PrintInt, ReadInt} where
            Return          x i -> Result x
            Migrate    host k i -> MigrateEffect host (k () i)
            PrintStr    str k i -> PrintStrEffect str (k () i)
            PrintInt      x k i -> PrintIntEffect x (k () i)
            ReadInt         k i -> 
                let key = StoreKey i in
                ReadIntEffect key (k (IntVar key) (i+1))
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
runCompTree (PrintIntEffect x comp, store) = do
    putStrLn $ ashow store x
    runCompTree (comp, store)
runCompTree (ReadIntEffect k comp, store) = do
    line <- getLine
    let store' = saveInt store k (read line)
    runCompTree (comp, store')

runMigrationComp :: MigrationComp -> IO Int
runMigrationComp comp = runCompTree (reifyComp 0 comp, emptyStore)

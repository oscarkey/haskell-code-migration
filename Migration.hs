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
data StoreValue = StoreIntValue Int 
    deriving (Show, Read)
type Store = Map.Map Int StoreValue

emptyStore :: Store
emptyStore = Map.empty

generalSave :: Store -> StoreKey a -> StoreValue -> Store
generalSave store (StoreKey k) x = Map.insert k x store

generalRetrieve :: Store -> StoreKey a -> StoreValue
generalRetrieve store (StoreKey k) =
    let v = Map.lookup k store in
        case v of Just u  -> u
                  Nothing -> error "No value associated with store location"

class Storeable a where
    save :: Store -> StoreKey a -> a -> Store
    retrieve :: Store -> StoreKey a -> a

instance Storeable Int where
    save store k x = generalSave store k (StoreIntValue x)
    retrieve store k = 
        let v = generalRetrieve store k
        in case v of StoreIntValue x -> x
                     _ -> error "Wrong type in store"


-- Abstract Int.
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

evalAbsInt :: Store -> AbsInt -> Int
evalAbsInt store (IntVal    x) = x
evalAbsInt store (IntVar    k) = retrieve store k
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


-- Computation tree.
data CompTree a = Result a
    | MigrateEffect HostName (CompTree a) 
    | PrintStrEffect String (CompTree a)
    | PrintIntEffect AbsInt (CompTree a)
    | ReadIntEffect (StoreKey Int) (CompTree a)
    deriving (Show, Read)


-- Handlers and reification.
[operation|Migrate  :: HostName -> ()|]
[operation|PrintStr :: String -> ()|]
[operation|PrintInt :: AbsInt -> ()|]
[operation|ReadInt  :: AbsInt|]

type MigrationComp a = ([handles|h {Migrate, PrintStr, PrintInt, ReadInt}|])
                        => Comp h a

[handler|
    ReifyComp a :: Int -> CompTree a
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
listenForComp :: (Show a, Read a) => IO a
listenForComp = listen (Host "127.0.0.1") portNum $ \(socket, socketAddress) -> do
    putStrLn "Listening for incoming connections..."
    accept socket $ \(socket, remoteAddress) -> do
        str <- recv socket 4096
        putStrLn "Received computation, running it"
        let (comp, store) = (read str :: (Show a, Read a) => (CompTree a, Store))
        runCompTree (comp, store)

sendComp :: (Show a, Read a) => HostName -> (CompTree a, Store) -> IO Int
sendComp hostName (comp, store) = do 
    connect hostName portNum $ \(socket, remoteAddress) -> do
        putStrLn $ "Sending computation to " ++ hostName
        send socket $ show (comp, store)


-- Interpreter.
runCompTree :: (Show a, Read a) => (CompTree a, Store) -> IO a
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
    let store' = save store k (read line)
    runCompTree (comp, store')

runMigrationComp :: (Show a, Read a) => MigrationComp a -> IO a
runMigrationComp comp = runCompTree (reifyComp 0 comp, emptyStore)

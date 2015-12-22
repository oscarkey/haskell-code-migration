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
data StoreValue = StoreNumValue Int deriving (Show, Read)
type Store = Map.Map Int StoreValue

saveNum :: Store -> StoreKey Int -> Int -> Store
saveNum store (StoreKey k) x = Map.insert k (StoreNumValue x) store

retrieveNum :: Store -> StoreKey Int -> Int
retrieveNum store (StoreKey k) = 
    let v = Map.lookup k store in
        case v of Just u  -> case u of StoreNumValue x -> x
                  Nothing -> error "No value associated with store location"


emptyStore :: Store
emptyStore = Map.empty


-- Abstract values.
data AbsNum = NumVal Int
            | NumVar (StoreKey Int)
            | OpPlus AbsNum AbsNum
            | OpMinus AbsNum AbsNum
            | OpMult AbsNum AbsNum
            | OpSig AbsNum
    deriving (Show, Read)

instance Num AbsNum where
    (NumVal x) + (NumVal y) = NumVal (x + y)
    x + y = OpPlus x y
    (NumVal x) - (NumVal y) = NumVal (x - y)
    x - y = OpMinus x y
    (NumVal x) * (NumVal y) = NumVal (x * y)
    x * y = OpMult x y
    negate x = (NumVal 0) - x
    signum x = OpSig x
    abs x = x * (signum x)
    fromInteger x = NumVal (fromInteger x)

evalAbsNum store (NumVal    x) = x
evalAbsNum store (NumVar    k) = retrieveNum store k
evalAbsNum store (OpPlus  x y) = (evalAbsNum store x) + (evalAbsNum store y)
evalAbsNum store (OpMinus x y) = (evalAbsNum store x) - (evalAbsNum store y)
evalAbsNum store (OpMult  x y) = (evalAbsNum store x) * (evalAbsNum store y)
evalAbsNum store (OpSig     x) = signum $ evalAbsNum store x


class AbsShow a where
    ashow :: Store -> a -> String

instance AbsShow AbsNum where
    ashow store x = show $ evalAbsNum store x

instance AbsShow Int where
    ashow store x = show x


-- Tree to represent computations.
data CompTree = Result Int
    | MigrateEffect HostName CompTree 
    | PrintStrEffect String CompTree
    | PrintNumEffect AbsNum CompTree
    | ReadNumEffect (StoreKey Int) CompTree deriving (Show,Read)


-- Handlers and reification.
[operation|Migrate  :: HostName -> ()|]
[operation|PrintStr :: String -> ()|]
[operation|PrintNum :: AbsNum -> ()|]
[operation|ReadNum  :: AbsNum|]

type MigrationComp = ([handles|h {Migrate, PrintStr, PrintNum, ReadNum}|]) 
                        => Comp h Int

[handler|
    ReifyComp :: Int -> CompTree
        handles {Migrate, PrintStr, PrintNum, ReadNum} where
            Return          x i -> Result x
            Migrate    host k i -> MigrateEffect host (k () i)
            PrintStr    str k i -> PrintStrEffect str (k () i)
            PrintNum      x k i -> PrintNumEffect x (k () i)
            ReadNum         k i -> 
                let key = StoreKey i in
                ReadNumEffect key (k (NumVar key) (i+1))
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
runCompTree (PrintNumEffect x comp, store) = do
    putStrLn $ ashow store x
    runCompTree (comp, store)
runCompTree (ReadNumEffect k comp, store) = do
    line <- getLine
    let store' = saveNum store k (read line)
    runCompTree (comp, store')

runMigrationComp :: MigrationComp -> IO Int
runMigrationComp comp = runCompTree (reifyComp 0 comp, emptyStore)

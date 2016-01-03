{-# LANGUAGE TypeFamilies,
    GADTs,
    RankNTypes,
    MultiParamTypeClasses,
    QuasiQuotes,
    FlexibleInstances,
    FlexibleContexts,
    OverlappingInstances,
    UndecidableInstances,
    ConstraintKinds,
    OverloadedLists,
    OverloadedStrings,
    StandaloneDeriving #-}

module Migration where

import Handlers
import DesugarHandlers
import Network.Simple.TCP (connect, listen, accept, HostPreference(Host), HostName)
import Network.Socket (recv, send)
import qualified Data.Map.Strict as Map
import Data.List
import Data.String
import GHC.Exts (IsList(Item, fromList, toList))

portNum = "8000"


-- Store.
data StoreKey a = StoreKey Int deriving (Show, Read)
data StoreValue = StoreIntValue Int 
                | StoreCharListValue [Char] 
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

instance Storeable [Char] where
    save store k x = generalSave store k (StoreCharListValue x)
    retrieve store k = 
        let v = generalRetrieve store k
        in case v of StoreCharListValue x -> x
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


-- Abstract lists.
data AbsList a = Nil
               | ListVar (StoreKey [a])
               | Cons a (AbsList a)
               | Append (AbsList a) (AbsList a)
               | Take Int (AbsList a)
               | Drop Int (AbsList a)
deriving instance Show a => Show (AbsList a)
deriving instance Read a => Read (AbsList a)

type AbsString = AbsList Char

instance IsList (AbsList a) where
    type Item (AbsList a) = a
    fromList     [] = Nil
    fromList (x:xs) = Cons x (fromList xs)
    toList xs = error "Cannot convert from AbsList to [] without store."

instance IsString (AbsList Char) where
    fromString s = fromList s

evalAbsList :: (Storeable [a]) => Store -> AbsList a -> [a]
evalAbsList store          Nil = []
evalAbsList store (ListVar  k) = retrieve store k
evalAbsList store (Cons  x xs) = x : (evalAbsList store xs)
evalAbsList store (Append x y) = (evalAbsList store x) ++ (evalAbsList store y)
evalAbsList store (Take  n xs) = take n (evalAbsList store xs)
evalAbsList store (Drop  n xs) = drop n (evalAbsList store xs)

acons :: a -> AbsList a -> AbsList a
acons x xs = Cons x xs

(+++) :: AbsList a -> AbsList a -> AbsList a
(+++) x y = Append x y

atake :: Int -> AbsList a -> AbsList a
atake n xs = Take n xs

adrop :: Int -> AbsList a -> AbsList a
adrop n xs = Drop n xs


-- Abstract Show.
class AbsShow a where
    ashow :: Store -> a -> String

instance AbsShow AbsInt where
    ashow store x = show $ evalAbsInt store x

instance (Storeable [a], Show a) => AbsShow (AbsList a) where
    ashow store x = show $ evalAbsList store x

instance AbsShow Int where
    ashow store x = show x


-- Computation tree.
data CompTree a = Result a
    | MigrateEffect HostName (CompTree a) 
    | PrintStrEffect AbsString (CompTree a)
    | PrintIntEffect AbsInt (CompTree a)
    | ReadStrEffect (StoreKey [Char]) (CompTree a)
    | ReadIntEffect (StoreKey Int) (CompTree a)
    deriving (Show, Read)


-- Handlers and reification.
[operation|Migrate  :: HostName -> ()|]
[operation|PrintStr :: AbsString -> ()|]
[operation|PrintInt :: AbsInt -> ()|]
[operation|ReadStr  :: AbsString|]
[operation|ReadInt  :: AbsInt|]

type MigrationComp a = ([handles|h {Migrate, PrintStr, PrintInt, ReadStr, ReadInt}|])
                        => Comp h a

[handler|
    ReifyComp a :: Int -> CompTree a
        handles {Migrate, PrintStr, PrintInt, ReadStr, ReadInt} where
            Return          x i -> Result x
            Migrate    host k i -> MigrateEffect host (k () i)
            PrintStr    str k i -> PrintStrEffect str (k () i)
            PrintInt      x k i -> PrintIntEffect x (k () i)
            ReadStr         k i -> 
                let key = StoreKey i in 
                ReadStrEffect key (k (ListVar key) (i+1))
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
    putStrLn $ ashow store str
    runCompTree (comp, store)
runCompTree (PrintIntEffect x comp, store) = do
    putStrLn $ ashow store x
    runCompTree (comp, store)
runCompTree (ReadStrEffect k comp, store) = do
    line <- getLine
    let store' = save store k line
    runCompTree (comp, store')
runCompTree (ReadIntEffect k comp, store) = do
    line <- getLine
    let store' = save store k (read line)
    runCompTree (comp, store')

runMigrationComp :: (Show a, Read a) => MigrationComp a -> IO a
runMigrationComp comp = runCompTree (reifyComp 0 comp, emptyStore)

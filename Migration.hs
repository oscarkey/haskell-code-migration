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

import Prelude hiding ((&&), (||), not, iterate)
import Handlers
import DesugarHandlers
import Network.Simple.TCP (connect, listen, accept, HostPreference(Host), HostName)
import Network.Socket (recv, send)
import qualified Data.Map.Strict as Map
import Data.List hiding (iterate)
import Data.String
import Data.Boolean
import Data.Foldable (traverse_)
import GHC.Exts (IsList(Item, fromList, toList))


-- Store.
data StoreKey a = StoreKey Int deriving (Show, Read)
type GenericStoreKey = Int
data StoreValue = StoreIntValue Int
                | StoreBoolValue Bool
                | StoreBoolListValue [Bool]
                | StoreStringValue String
                | StoreAbsStringValue AbsString
                | StoreStringListValue [String]
                | StoreAbsStringListValue [AbsString]
    deriving (Show, Read)
type Store = Map.Map GenericStoreKey StoreValue

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
                     _ -> error "Wrong type in store, expected Int"

instance Storeable Bool where
    save store k x = generalSave store k (StoreBoolValue x)
    retrieve store k = 
        let v = generalRetrieve store k
        in case v of StoreBoolValue x -> x
                     _ -> error "Wrong type in store, expected Bool"

instance Storeable [Bool] where
    save store k x = generalSave store k (StoreBoolListValue x)
    retrieve store k = 
        let v = generalRetrieve store k
        in case v of StoreBoolListValue x -> x
                     _ -> error "Wrong type in store, expected [Bool]"

instance Storeable String where
    save store k x = generalSave store k (StoreStringValue x)
    retrieve store k = 
        let v = generalRetrieve store k
        in case v of StoreStringValue x -> x
                     _ -> error "Wrong type in store, expected String"

instance Storeable AbsString where
    save store k x = generalSave store k (StoreAbsStringValue x)
    retrieve store k =
        let v = generalRetrieve store k
        in case v of StoreAbsStringValue x -> x
                     _ -> error "Wrong type in store, expected AbsString"

instance Storeable [String] where
    save store k x = generalSave store k (StoreStringListValue x)
    retrieve store k =
        let v = generalRetrieve store k
        in case v of StoreStringListValue x -> x
                     _ -> error "Wrong type in store, expected [String]"

instance Storeable [AbsString] where
    save store k x = generalSave store k (StoreAbsStringListValue x)
    retrieve store k =
        let v = generalRetrieve store k
        in case v of StoreAbsStringListValue x -> x
                     _ -> error "Wrong type in store, expected [AbsString]"


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


-- Abstract Bool.
data AbsBool = BoolVal Bool
             | BoolVar (StoreKey Bool)
             | And AbsBool AbsBool
             | Or AbsBool AbsBool
             | Not AbsBool
    deriving (Show, Read)

instance BoolValue AbsBool where
    true = BoolVal True
    false = BoolVal False

instance Boolean AbsBool where
    (&&) x y = And x y
    (||) x y = Or x y
    not x = Not x

evalAbsBool :: Store -> AbsBool -> Bool
evalAbsBool store (BoolVal x) = x
evalAbsBool store (BoolVar k) = retrieve store k
evalAbsBool store (And   x y) = (evalAbsBool store x) && (evalAbsBool store y)
evalAbsBool store (Or    x y) = (evalAbsBool store x) || (evalAbsBool store y)
evalAbsBool store (Not     x) = not (evalAbsBool store x)


-- Abstract lists.
data AbsList a = Nil
               | ListVal [a]
               | ListVar (StoreKey [a])
               | Cons a (AbsList a)
               | Append (AbsList a) (AbsList a)
               | Take Int (AbsList a)
               | Drop Int (AbsList a)
               | Tail (AbsList a)
deriving instance Show a => Show (AbsList a)
deriving instance Read a => Read (AbsList a)

type AbsString = AbsList Char

instance IsList (AbsList a) where
    type Item (AbsList a) = a
    fromList [] = Nil
    fromList xs = ListVal xs
    toList xs = error "Cannot convert from AbsList to [] without store."

instance IsString AbsString where
    fromString s = fromList s

evalAbsList :: (Storeable [a]) => Store -> AbsList a -> [a]
evalAbsList store           Nil = []
evalAbsList store (ListVal  xs) = xs
evalAbsList store (ListVar   k) = retrieve store k
evalAbsList store (Cons   x xs) = x : (evalAbsList store xs)
evalAbsList store (Append  x y) = (evalAbsList store x) ++ (evalAbsList store y)
evalAbsList store (Take   n xs) = take n (evalAbsList store xs)
evalAbsList store (Drop   n xs) = drop n (evalAbsList store xs)
evalAbsList store (Tail     xs) = let xs' = evalAbsList store xs
                                  in case xs' of [] -> []
                                                 (y:ys) -> ys

acons :: a -> AbsList a -> AbsList a
acons x xs = Cons x xs

(+++) :: AbsList a -> AbsList a -> AbsList a
(+++) x y = Append x y

atake :: Int -> AbsList a -> AbsList a
atake n xs = Take n xs

adrop :: Int -> AbsList a -> AbsList a
adrop n xs = Drop n xs

tl :: AbsList a -> AbsList a
tl xs = Tail xs


-- Abstract iteration
forEach :: (AbsString -> MigrationComp ()) -> AbsList AbsString -> MigrationComp ()
forEach f xs = do
    fresh <- freshVar
    let k = StoreKey fresh
    let rf = reifyComp (fresh*10000) (f (ListVar k))
    iterate (rf, xs, k)

forEvery :: Port -> UnitCompTree -> [AbsString] -> Store -> StoreKey String -> IO ()
forEvery port f [] store k = return ()
forEvery port f (x:xs) store k = do
    let str = evalAbsList store x
    let store' = save store k str
    runCompTree port (store', f)
    forEvery port f xs store k


-- Abstract Show.
class AbsShow a where
    ashow :: Store -> a -> String

instance AbsShow AbsInt where
    ashow store x = show $ evalAbsInt store x

instance AbsShow AbsBool where
    ashow store x = show $ evalAbsBool store x

instance (Storeable [a], Show a) => AbsShow (AbsList a) where
    ashow store x = show $ evalAbsList store x

instance AbsShow Int where
    ashow store x = show x


-- Abstract Eq.
class AbsEq a where
    (===) :: a -> a -> MigrationComp Bool

instance AbsEq AbsInt where
    (===) x y = equal (EqableAbsInt x, EqableAbsInt y)

instance AbsEq AbsBool where
    (===) x y = equal (EqableAbsBool x, EqableAbsBool y)

instance AbsEq Int where
    (===) x y = return $ x==y

instance AbsEq AbsString where
    (===) x y = equal (EqableAbsString x, EqableAbsString y)

data AbsEqable = EqableAbsInt AbsInt 
               | EqableAbsBool AbsBool
               | EqableAbsString AbsString
    deriving (Show, Read)

evalAbsEqable :: Store -> (AbsEqable,AbsEqable) -> Bool
evalAbsEqable store (EqableAbsInt x, EqableAbsInt y) = 
                                                    (evalAbsInt store x) == (evalAbsInt store y)
evalAbsEqable store (EqableAbsBool x, EqableAbsBool y) = 
                                                    (evalAbsBool store x) == (evalAbsBool store y)
evalAbsEqable store (EqableAbsString x, EqableAbsString y) = 
                                                    (evalAbsList store x) == (evalAbsList store y)
evalAbsEqable store (_,_) = error "Mismatched Eqable constructors"


-- Computation tree.
data CompTree a = Result a
    | MigrateEffect (HostName, Port) (CompTree a)
    | PrintStrEffect AbsString (CompTree a)
    | PrintStrListEffect (AbsList AbsString) (CompTree a)
    | PrintIntEffect AbsInt (CompTree a)
    | ReadStrEffect (StoreKey [Char]) (CompTree a)
    | ReadIntEffect (StoreKey Int) (CompTree a)
    | EqualEffect (AbsEqable, AbsEqable) (CompTree a) (CompTree a)
    | IterateEffect (CompTree (), AbsList AbsString, StoreKey String) (CompTree a)
    | HdEffect (AbsList AbsString) (StoreKey String) (CompTree a) (CompTree a)
    deriving (Show, Read)
-- UnitCompTree sometimes has to be used in place of CompTree (), this needs to be investigated.
type UnitCompTree = CompTree ()


-- Handlers and reification.
[operation|Migrate      :: (HostName, Port) -> ()|]
[operation|PrintStr     :: AbsString -> ()|]
[operation|PrintStrList :: AbsList AbsString -> ()|]
[operation|PrintInt     :: AbsInt -> ()|]
[operation|ReadStr      :: AbsString|]
[operation|ReadInt      :: AbsInt|]
[operation|Equal        :: (AbsEqable, AbsEqable) -> Bool|]
[operation|Iterate      :: (UnitCompTree, AbsList AbsString, StoreKey String) -> ()|]
[operation|Hd           :: AbsList AbsString -> Maybe AbsString|]
[operation|FreshVar     :: GenericStoreKey|]

type MigrationComp a = ([handles|h {Migrate, PrintStr, PrintStrList, PrintInt, ReadStr, ReadInt, 
                                    Equal, Iterate, Hd, FreshVar}|])
                        => Comp h a

[handler|
    ReifyComp a :: GenericStoreKey -> CompTree a
        handles {Migrate, PrintStr, PrintStrList, PrintInt, ReadStr, ReadInt, Equal, Iterate, Hd,
                 FreshVar} where
            Return            x i -> Result x
            Migrate    (h, p) k i -> MigrateEffect (h, p) (k () i)
            PrintStr      str k i -> PrintStrEffect str (k () i)
            PrintStrList strs k i -> PrintStrListEffect strs (k () i)
            PrintInt        x k i -> PrintIntEffect x (k () i)
            ReadStr           k i -> 
                let key = StoreKey i
                in ReadStrEffect key (k (ListVar key) (i+1))
            ReadInt           k i -> 
                let key = StoreKey i
                in ReadIntEffect key (k (IntVar key) (i+1))
            Equal       (x,y) k i -> EqualEffect (x,y) (k True i) (k False i)
            Iterate  (f,xs,x) k i -> IterateEffect (f,xs,x) (k () i)
            Hd             xs k i ->
                let key = StoreKey i
                    x = Just (ListVar key)
                in HdEffect xs key (k x (i+1)) (k Nothing i)
            FreshVar          k i -> k i (i+1)
|]


-- Networking.
type Port = String

listenForComp :: (Show a, Read a) => Port -> IO a
listenForComp port = listen (Host "127.0.0.1") port $ \(socket, socketAddress) -> do
    putStrLn "Listening for incoming connections..."
    accept socket $ \(socket, remoteAddress) -> do
        str <- recv socket 4096
        putStrLn "Received computation, running it"
        let (store, comp) = (read str :: (Show a, Read a) => (Store, CompTree a))
        (store', x) <- runCompTree port (store, comp)
        return x

sendComp :: (Show a, Read a) => (HostName, Port) -> (Store, CompTree a) -> IO Int
sendComp (hostName, port) (store, comp) = do 
    connect hostName port $ \(socket, remoteAddress) -> do
        putStrLn $ "Sending computation to " ++ hostName
        send socket $ show (store, comp)


-- Interpreter.
runCompTree :: (Show a, Read a) => Port -> (Store, CompTree a) -> IO (Store, a)
runCompTree port (store, effect) = case effect of
    Result x -> return (store, x)
    MigrateEffect (dhost, dport) comp -> do
        sendComp (dhost, dport) (store, comp)
        listenForComp port
    PrintStrEffect str comp -> do
        putStrLn $ ashow store str
        runCompTree port (store, comp)
    PrintStrListEffect strs comp -> do
        let strs' = evalAbsList store strs
        traverse_ (\str -> putStrLn $ ashow store str) strs'
        runCompTree port (store, comp)
    PrintIntEffect x comp -> do
        putStrLn $ ashow store x
        runCompTree port (store, comp)
    ReadStrEffect k comp -> do
        line <- getLine
        let store' = save store k line
        runCompTree port (store', comp)
    ReadIntEffect k comp -> do
        line <- getLine
        let store' = save store k (read line)
        runCompTree port (store', comp)
    EqualEffect (x,y) compt compf -> 
        if evalAbsEqable store (x,y) then runCompTree port (store, compt) 
                                     else runCompTree port (store, compf)
    IterateEffect (f,xs,k) comp -> do
        let xs' = evalAbsList store xs
        forEvery port f xs' store k
        runCompTree port (store, comp)
    HdEffect xs k compt compf -> 
        let xs' = evalAbsList store xs
        in case xs' of [] -> runCompTree port (store, compf)
                       (x:xs) -> do
                            let x' = evalAbsList store x
                                store' = save store k x'
                            runCompTree port (store', compt)

runMigrationComp :: (Show a, Read a) => Port -> MigrationComp a -> IO a
runMigrationComp port comp = do
    (store, x) <- runCompTree port (emptyStore, reifyComp 0 comp)
    return x

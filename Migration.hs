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
    StandaloneDeriving,
    FunctionalDependencies #-}

module Migration where

import Prelude hiding ((&&), (||), not, iterate)
import Handlers
import DesugarHandlers
import Network.Simple.TCP (connect, serve, HostPreference(Host), HostName)
import Network.Socket (send, socketToHandle)
import System.IO 
import System.Directory (getDirectoryContents)
import GHC.IO.Handle 
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
                | StoreCharValue Char
                | StoreBoolListValue [Bool]
                | StoreStringValue String
                | StoreAbsStringValue AbsString
                | StoreStringListValue [String]
                | StoreAbsStringListValue [AbsString]
    deriving (Show, Read)
type Store = Map.Map GenericStoreKey StoreValue

emptyStore :: Store
emptyStore = Map.empty

genericSave :: Store -> StoreKey a -> StoreValue -> Store
genericSave store (StoreKey k) x = Map.insert k x store

genericRetrieve :: Store -> StoreKey a -> StoreValue
genericRetrieve store (StoreKey k) =
    let v = Map.lookup k store in
        case v of Just u  -> u
                  Nothing -> error "No value associated with store location"

class Storeable a where
    save :: Store -> StoreKey a -> a -> Store
    retrieve :: Store -> StoreKey a -> a

instance Storeable Int where
    save store k x = genericSave store k (StoreIntValue x)
    retrieve store k = 
        let v = genericRetrieve store k
        in case v of StoreIntValue x -> x
                     _ -> error "Wrong type in store, expected Int"

instance Storeable Bool where
    save store k x = genericSave store k (StoreBoolValue x)
    retrieve store k = 
        let v = genericRetrieve store k
        in case v of StoreBoolValue x -> x
                     _ -> error "Wrong type in store, expected Bool"

instance Storeable Char where
    save store k x = genericSave store k (StoreCharValue x)
    retrieve store k = 
        let v = genericRetrieve store k
        in case v of StoreCharValue x -> x
                     _ -> error "Wrong type in store, expected Bool"

instance Storeable [Bool] where
    save store k x = genericSave store k (StoreBoolListValue x)
    retrieve store k = 
        let v = genericRetrieve store k
        in case v of StoreBoolListValue x -> x
                     _ -> error "Wrong type in store, expected [Bool]"

instance Storeable String where
    save store k x = genericSave store k (StoreStringValue x)
    retrieve store k = 
        let v = genericRetrieve store k
        in case v of StoreStringValue x -> x
                     _ -> error "Wrong type in store, expected String"

instance Storeable AbsString where
    save store k x = genericSave store k (StoreAbsStringValue x)
    retrieve store k =
        let v = genericRetrieve store k
        in case v of StoreAbsStringValue x -> x
                     _ -> error "Wrong type in store, expected AbsString"

instance Storeable [String] where
    save store k x = genericSave store k (StoreStringListValue x)
    retrieve store k =
        let v = genericRetrieve store k
        in case v of StoreStringListValue x -> x
                     _ -> error "Wrong type in store, expected [String]"

instance Storeable [AbsString] where
    save store k x = genericSave store k (StoreAbsStringListValue x)
    retrieve store k =
        let v = genericRetrieve store k
        in case v of StoreAbsStringListValue x -> x
                     _ -> error "Wrong type in store, expected [AbsString]"

-- Abstract values.
class Abstract a c | a -> c where
    eval :: Store -> a -> c
    toAbs :: c -> a

-- While these types are not abstract, it is helpful to map them to themselves so we can eval them.
instance Abstract Int Int where
    eval _ x = x
    toAbs x = x


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

instance Abstract AbsInt Int where
    eval store (IntVal    x) = x
    eval store (IntVar    k) = retrieve store k
    eval store (OpPlus  x y) = (eval store x) + (eval store y)
    eval store (OpMinus x y) = (eval store x) - (eval store y)
    eval store (OpMult  x y) = (eval store x) * (eval store y)
    eval store (OpSig     x) = signum $ eval store x
    toAbs x = IntVal x


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

instance Abstract AbsBool Bool where
    eval store (BoolVal x) = x
    eval store (BoolVar k) = retrieve store k
    eval store (And   x y) = (eval store x) && (eval store y)
    eval store (Or    x y) = (eval store x) || (eval store y)
    eval store (Not     x) = not (eval store x)
    toAbs x = BoolVal x


-- Abstract Char.
data AbsChar = CharVal Char
             | CharVar (StoreKey Char)
    deriving (Show, Read)

instance Abstract AbsChar Char where
    eval store (CharVal x) = x
    eval store (CharVar k) = retrieve store k
    toAbs x = CharVal x


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

instance (Storeable [a]) => Abstract (AbsList a) [a] where
    eval store           Nil = []
    eval store (ListVal  xs) = xs
    eval store (ListVar   k) = retrieve store k
    eval store (Cons   x xs) = x : (eval store xs)
    eval store (Append  x y) = (eval store x) ++ (eval store y)
    eval store (Take   n xs) = take n (eval store xs)
    eval store (Drop   n xs) = drop n (eval store xs)
    eval store (Tail     xs) = let xs' = eval store xs
                                  in case xs' of [] -> []
                                                 (y:ys) -> ys
    toAbs x = ListVal x

acons :: a -> AbsList a -> AbsList a
acons x xs = Cons x xs

(+++) :: AbsList a -> AbsList a -> AbsList a
(+++) x y = Append x y

atake :: Int -> AbsList a -> AbsList a
atake n xs = Take n xs

adrop :: Int -> AbsList a -> AbsList a
adrop n xs = Drop n xs

tl :: AbsList a -> AbsList a
tl (ListVal []) = ListVal []
tl (ListVal (x:xs)) = ListVal xs
tl xs = Tail xs


-- Abstract iteration
forEach :: (AbsString -> MigrationComp ()) -> AbsList AbsString -> MigrationComp ()
forEach f xs = do
    fresh <- freshVar
    let k = StoreKey fresh
        rf = reifyComp (fresh*10000) (f (ListVar k))
    iterate (rf, xs, k)

forEvery :: UnitCompTree -> [AbsString] -> Store -> StoreKey String -> IO ()
forEvery f [] store k = return ()
forEvery f (x:xs) store k = do
    let str = eval store x
        store' = save store k str
    runCompTree (store', f)
    forEvery f xs store k


-- Abstract Show.
class AbsShow a where
    ashow :: Store -> a -> String

instance AbsShow AbsInt where
    ashow store x = show $ eval store x

instance AbsShow AbsBool where
    ashow store x = show $ eval store x

instance AbsShow AbsString where
    ashow store x = eval store x

instance (Storeable [a], Show a) => AbsShow (AbsList a) where
    ashow store x = show $ eval store x

instance AbsShow Int where
    ashow store x = show x


-- Abstract Eq.
class AbsEq a where
    (===) :: a -> a -> MigrationComp Bool

instance AbsEq AbsInt where
    (===) x y = equal (EqableAbsInt x, EqableAbsInt y)

instance AbsEq AbsBool where
    (===) x y = equal (EqableAbsBool x, EqableAbsBool y)

instance AbsEq AbsChar where
    (===) x y = equal (EqableAbsChar x, EqableAbsChar y)

instance AbsEq Int where
    (===) x y = return $ x==y

instance AbsEq AbsString where
    (===) x y = equal (EqableAbsString x, EqableAbsString y)

data AbsEqable = EqableAbsInt AbsInt 
               | EqableAbsBool AbsBool
               | EqableAbsChar AbsChar
               | EqableAbsString AbsString
    deriving (Show, Read)

evalAbsEqable :: Store -> (AbsEqable,AbsEqable) -> Bool
evalAbsEqable store (EqableAbsInt x, EqableAbsInt y) = (eval store x) == (eval store y)
evalAbsEqable store (EqableAbsBool x, EqableAbsBool y) = (eval store x) == (eval store y)
evalAbsEqable store (EqableAbsChar x, EqableAbsChar y) = (eval store x) == (eval store y)
evalAbsEqable store (EqableAbsString x, EqableAbsString y) = (eval store x) == (eval store y)
evalAbsEqable store (_,_) = error "Mismatched Eqable constructors"


-- Computation tree.
data CompTree a = Result a
    | MigrateEffect (AbsHostName, AbsPort) (CompTree a)
    | PrintStrEffect AbsString (CompTree a)
    | PrintStrListEffect (AbsList AbsString) (CompTree a)
    | PrintIntEffect AbsInt (CompTree a)
    | ReadStrEffect (StoreKey [Char]) (CompTree a)
    | ReadIntEffect (StoreKey Int) (CompTree a)
    | AskEffect AbsString (CompTree a) (CompTree a)
    | ReadFlEffect AbsString (StoreKey [Char]) (CompTree a)
    | ListFlsEffect (StoreKey [AbsString]) (CompTree a)
    | EqualEffect (AbsEqable, AbsEqable) (CompTree a) (CompTree a)
    | IterateEffect (CompTree (), AbsList AbsString, StoreKey String) (CompTree a)
    | HdEffect (AbsList AbsString) (StoreKey String) (CompTree a) (CompTree a)
    deriving (Show, Read)
-- UnitCompTree sometimes has to be used in place of CompTree (), this needs to be investigated.
type UnitCompTree = CompTree ()


-- Handlers and reification.
[operation|Migrate      :: (AbsHostName, AbsPort) -> ()|]
[operation|PrintStr     :: AbsString -> ()|]
[operation|PrintStrList :: AbsList AbsString -> ()|]
[operation|PrintInt     :: AbsInt -> ()|]
[operation|ReadStr      :: AbsString|]
[operation|ReadInt      :: AbsInt|]
[operation|ReadFl       :: AbsString -> AbsString|]
[operation|Ask          :: AbsString -> Bool|]
[operation|ListFls      :: AbsList AbsString|]
[operation|Equal        :: (AbsEqable, AbsEqable) -> Bool|]
[operation|Iterate      :: (UnitCompTree, AbsList AbsString, StoreKey String) -> ()|]
[operation|Hd           :: AbsList AbsString -> Maybe AbsString|]
[operation|FreshVar     :: GenericStoreKey|]

type MigrationComp a = ([handles|h {Migrate, PrintStr, PrintStrList, PrintInt, ReadStr, ReadInt, 
                                    Ask, ReadFl, ListFls, Equal, Iterate, Hd, FreshVar}|])
                        => Comp h a

[handler|
    ReifyComp a :: GenericStoreKey -> CompTree a
        handles {Migrate, PrintStr, PrintStrList, PrintInt, ReadStr, ReadInt, Ask, ReadFl, 
                 ListFls, Equal, Iterate, Hd, FreshVar} where
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
            Ask             q k i -> AskEffect q (k True i) (k False i)
            ReadFl       file k i ->
                let key = StoreKey i
                in ReadFlEffect file key (k (ListVar key) (i+1))
            ListFls           k i ->
                let key = StoreKey i
                in ListFlsEffect key (k (ListVar key) (i+1))
            Equal       (x,y) k i -> EqualEffect (x,y) (k True i) (k False i)
            Iterate  (f,xs,x) k i -> IterateEffect (f,xs,x) (k () i)
            Hd             xs k i ->
                let key = StoreKey i
                    x = Just (ListVar key)
                in HdEffect xs key (k x (i+1)) (k Nothing i)
            FreshVar          k i -> k i (i+1)
|]


-- Networking.
type AbsHostName = AbsString
type Port = String
type AbsPort = AbsString

listenForComp :: Port -> IO ()
listenForComp port = do
    putStrLn "Listening for incoming connections..."
    serve (Host "127.0.0.1") port $ \(socket, remoteAddress) -> do
        handle <- socketToHandle socket ReadMode
        hSetBuffering handle LineBuffering
        str <- hGetLine handle
        hClose handle
        putStrLn "Received computation, running it"
        let (store, comp) = (read str :: (Store, CompTree ()))
        runCompTree (store, comp)
        return ()

sendComp :: (Show a, Read a) => (HostName, Port) -> (Store, CompTree a) -> IO Int
sendComp (hostName, port) (store, comp) = do 
    connect hostName port $ \(socket, remoteAddress) -> do
        putStrLn $ "Sending computation to " ++ hostName
        send socket $ show (store, comp)


-- Interpreter.
runCompTree :: (Show a, Read a) => (Store, CompTree a) -> IO (Maybe (Store, a))
runCompTree (store, effect) = case effect of
    Result x -> return $ Just (store, x)
    MigrateEffect (host, port) comp -> do
        let host' = eval store host
            port' = eval store port
        sendComp (host', port') (store, comp)
        return Nothing
    PrintStrEffect str comp -> do
        putStrLn $ ashow store str
        runCompTree (store, comp)
    PrintStrListEffect strs comp -> do
        let strs' = eval store strs
        traverse_ (\str -> putStrLn $ ashow store str) strs'
        runCompTree (store, comp)
    PrintIntEffect x comp -> do
        putStrLn $ ashow store x
        runCompTree (store, comp)
    ReadStrEffect k comp -> do
        line <- getLine
        let store' = save store k line
        runCompTree (store', comp)
    ReadIntEffect k comp -> do
        value <- readLn
        let store' = save store k value
        runCompTree (store', comp)
    ReadFlEffect file k comp -> do
        let file' = eval store file
        text <- readFile file'
        let store' = save store k text
        runCompTree (store', comp)
    ListFlsEffect k comp -> do
        files <- getDirectoryContents "."
        let absFiles = map (\s -> toAbs s) files
        let store' = save store k absFiles
        runCompTree (store', comp)
    EqualEffect (x,y) compt compf -> 
        if evalAbsEqable store (x,y) then runCompTree (store, compt) 
                                     else runCompTree (store, compf)
    IterateEffect (f,xs,k) comp -> do
        let xs' = eval store xs
        forEvery f xs' store k
        runCompTree (store, comp)
    HdEffect xs k compt compf -> 
        let xs' = eval store xs
        in case xs' of [] -> runCompTree (store, compf)
                       (x:xs) -> do
                            let x' = eval store x
                                store' = save store k x'
                            runCompTree (store', compt)

runMigrationComp :: Port -> MigrationComp () -> IO ()
runMigrationComp port comp = do
    let comp' = reifyComp 0 comp
    runCompTree (emptyStore, comp')
    listenForComp port
    return ()

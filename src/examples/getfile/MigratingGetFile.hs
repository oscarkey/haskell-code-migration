{-# LANGUAGE
    FlexibleContexts,
    ConstraintKinds,
    OverloadedStrings,
    OverloadedLists,
    RankNTypes #-}

import Migration
import System.Environment (getArgs)
import Network.Simple.TCP (HostName)

type AbsFileName = AbsString

authServer = "127.0.0.1"
clientPort = "8001"
authPort = "8002"
filePort = "8003"

listFilesComp :: Port -> AbsHostName -> MigrationComp ()
listFilesComp port fileHost = do
    (username, password) <- getUserPass
    migrate (authServer, authPort)
    printStr $ username +++ " is trying to list the files on " +++ fileHost
    migrate (fileHost, filePort)
    files <- listFls
    migrate ("127.0.0.1", clientPort)
    forEach (\fileName -> printStr fileName) files

getFilesComp :: Port -> AbsHostName -> AbsList AbsFileName -> MigrationComp ()
getFilesComp port fileHost fileNames = do
    (username, password) <- getUserPass
    migrate (authServer, authPort)
    printStr $ username +++ " is trying to access files on server " +++ fileHost
    migrate (fileHost, filePort)
    files <- readFiles fileNames
    migrate ("127.0.0.1", clientPort)
    forEach (\file -> printStr file) files

getUserPass :: MigrationComp (AbsString, AbsString)
getUserPass = do
    printStr "Please enter your username:"
    username <- readStr
    printStr "Please enter your password:"
    password <- readStr
    return (username, password)

readFiles :: AbsList AbsFileName -> MigrationComp (AbsList AbsString)
readFiles fileNames =
    let limited 0 _ = return Nil
        limited n files = do
            file <- hd files
            case file of Nothing -> return Nil
                         Just f -> do
                            text <- readFl f
                            rest <- limited (n-1) (tl files)
                            return $ acons text rest
    in limited 10 fileNames

main :: IO ()
main = do
    args <- getArgs
    case args of
        [port, fileHost] -> runMigrationComp port $ listFilesComp port (toAbs fileHost)
        (port : fileHost : files) -> 
            let absFiles = toAbs $ map (\file -> toAbs file) files
            in runMigrationComp port $ getFilesComp port (toAbs fileHost) absFiles
        _ -> error "Please provide arguments [port] [server address] ([file names])"

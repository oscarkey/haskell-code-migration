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

authServer = "127.0.0.1" :: AbsString
authPort = "8002" :: AbsPort
filePort = "8003" :: AbsPort

listFilesComp :: Port -> AbsHostName -> MigrationComp ()
listFilesComp port fileHost = do
    printStr "Please enter your username:"
    username <- readStr
    printStr "Please enter your password:"
    password <- readStr
    migrate (authServer, authPort)
    printStr $ username +++ " is trying to list the files on " +++ fileHost
    migrate (fileHost, filePort)
    files <- listFls
    migrate ("127.0.0.1", "8001")
    forEach (\fileName -> printStr fileName) files


getFileComp :: Port -> AbsHostName -> AbsList AbsFileName -> MigrationComp ()
getFileComp port fileHost fileNames = do
    printStr "Please enter your username:"
    username <- readStr
    printStr "Please enter your password:"
    password <- readStr
    migrate (authServer, authPort)
    printStr $ username +++ " is trying to access files on server " +++ fileHost
    migrate (fileHost, filePort)
    files <- readFiles fileNames
    migrate ("127.0.0.1", "8001")
    forEach (\file -> printStr file) files

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
    in limited (10::Int) fileNames

main :: IO ()
main = do
    args <- getArgs
    case args of
        [port, fileHost] -> runMigrationComp port $ listFilesComp port (toAbs fileHost)
        (port : fileHost : files) -> 
            let absFiles = toAbs $ map (\file -> toAbs file) files
            in runMigrationComp port $ getFileComp port (toAbs fileHost) absFiles
        _ -> error "Please provide arguments [port] [server address] ([file name])"

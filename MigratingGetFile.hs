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


getFileComp :: Port -> AbsHostName -> AbsFileName -> MigrationComp ()
getFileComp port fileHost fileName = do
	printStr "Please enter your username:"
	username <- readStr
	printStr "Please enter your password:"
	password <- readStr
	migrate (authServer, authPort)
	printStr $ username +++ " is trying to access file " +++ fileName +++ " on server " +++ fileHost
	migrate (fileHost, filePort)
	file <- readFl fileName
	migrate ("127.0.0.1", "8001")
	printStr file


main :: IO ()
main = do
	args <- getArgs
	case args of
		[port, fileHost] -> runMigrationComp port $ listFilesComp port (toAbs fileHost)
		[port, fileHost, fileName] -> 
			runMigrationComp port $ getFileComp port (toAbs fileHost) (toAbs fileName)
		_ -> error "Please provide arguments [port] [server address] ([file name])"

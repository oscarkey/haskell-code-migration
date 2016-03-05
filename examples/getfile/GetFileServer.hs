{-# LANGUAGE NamedFieldPuns #-}

import Network.Simple.TCP hiding (send)
import Network.Socket (socketToHandle, send)
import Control.Monad (when, mapM)
import GHC.IO.Handle
import System.IO
import System.Directory (getDirectoryContents)
import GetFileCommon

-- Receive the request, 
receiveRequest :: (Socket, SockAddr) -> IO ()
receiveRequest (socket, remoteAddress) = do
    -- Receive and deserialize the request.
    handle <- socketToHandle socket ReadMode
    hSetBuffering handle LineBuffering
    received <- hGetLine handle
    let fileRequest = read received
    -- Find out what kind of request this is.
    (clientHost, clientPort, response) <- case fileRequest of 
        FileRequest {clientHost, clientPort, fileNames} -> do
            files <- mapM (\file -> readFile file) fileNames
            return (clientHost, clientPort, FileResponse files)
        ListRequest {clientHost, clientPort} -> do
            files <- getDirectoryContents "."
            return (clientHost, clientPort, ListResponse files)
    -- Send the response to the client.
    connect clientHost clientPort $ \(socket, _) -> do
        send socket $ show response     
    return ()


main :: IO ()
main = do
    putStrLn "Listening for file requests..."
    serve HostAny filePort receiveRequest

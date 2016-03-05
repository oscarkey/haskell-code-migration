import System.Environment (getArgs)
import Network.Simple.TCP hiding (send)
import Network.Socket (socketToHandle, send)
import GHC.IO.Handle
import System.IO 
import Data.Foldable (traverse_)
import GetFileCommon

makeRequest :: Port -> HostName -> DataRequest -> IO ()
makeRequest port fileHost request = do
    -- Get the username and password.
    putStrLn "Please enter your username:"
    username <- getLine
    putStrLn "Please enter your password:"
    password <- getLine
    -- Build and send the request.
    let authRequest = AuthRequest {username = username,
                                   password = password,
                                   fileHost = fileHost,
                                   dataRequest = request}
    connect authHost authPort $ \(socket, remoteAddress) -> do
        send socket $ show authRequest
    -- Wait for the response and print it.
    listen HostAny port $ \(socket, _) -> do
        accept socket $ \(socket, _) -> do
            handle <- socketToHandle socket ReadMode
            hSetBuffering handle LineBuffering
            received <- hGetLine handle
            hClose handle
            let response = read received
            case response of FileResponse files -> traverse_ (\file -> putStrLn file) files
                             ListResponse files -> traverse_ (\file -> putStrLn file) files 
    return ()


main :: IO ()
main = do
    args <- getArgs
    case args of
        [port, fileHost] -> makeRequest port fileHost $
                                ListRequest {clientHost = "127.0.0.1", clientPort = "8001"} 
        (port : fileHost : files) -> makeRequest port fileHost $
                FileRequest {clientHost = "127.0.0.1", clientPort = "8001", fileNames = files}
        _ -> error "Please provide arguments [port] [server address] ([file names])"

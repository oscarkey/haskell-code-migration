import Network.Simple.TCP hiding (send)
import Network.Socket (socketToHandle, send)
import Control.Monad (when)
import GHC.IO.Handle
import System.IO
import GetFileCommon

-- Receive auth requests, check them and forward the data request (or not).
receiveAuthRequest :: (Socket, SockAddr) -> IO ()
receiveAuthRequest (socket, remoteAddress) = do
    -- Receive and deserialize the request.
    handle <- socketToHandle socket ReadMode
    hSetBuffering handle LineBuffering
    received <- hGetLine handle
    hClose handle
    let authRequest = read received
    putStrLn $ (username authRequest) ++ " is trying to access server " ++ (fileHost authRequest)
    -- Check the username and password.
    when (checkAuthed authRequest) $ do
        -- Forward the request to the relevant file server.
        connect (fileHost authRequest) filePort $ \(socket, _) -> do
            send socket $ show (dataRequest authRequest)
        return ()

checkAuthed :: AuthRequest -> Bool
checkAuthed request = True

main :: IO ()
main = do
    putStrLn "Listening for auth requests..."
    serve HostAny authPort receiveAuthRequest

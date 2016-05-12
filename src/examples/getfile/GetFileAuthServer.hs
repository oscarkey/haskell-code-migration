import Network.Simple.TCP hiding (send)
import Network.Socket (socketToHandle, send)
import Control.Monad (when)
import GHC.IO.Handle
import System.IO
import GetFileCommon
import System.Environment (getArgs)

-- Receive auth requests, check them and forward the data request (or not).
receiveAuthRequest :: String -> (Socket, SockAddr) -> IO ()
receiveAuthRequest auth (socket, remoteAddress) = do
    -- Receive and deserialize the request.
    handle <- socketToHandle socket ReadMode
    hSetBuffering handle LineBuffering
    received <- hGetLine handle
    hClose handle
    let authRequest = read received
    putStrLn $ (username authRequest) ++ " is trying to access server " ++ (fileHost authRequest)
    -- Check the username and password.
    when (checkAuthed auth authRequest) $ do
        -- Forward the request to the relevant file server.
        connect (fileHost authRequest) filePort $ \(socket, _) -> do
            send socket $ show (dataRequest authRequest)
        return ()

checkAuthed :: String -> AuthRequest -> Bool
checkAuthed auth request = 
    let authTry = (username request) ++ (password request)
    in auth == authTry

main :: IO ()
main = do
    args <- getArgs
    if length args == 1 then do
        putStrLn "Listening for auth requests..."
        let (auth:_) = args
        serve HostAny authPort (receiveAuthRequest auth)
    else putStrLn "Usage ./GetFileAuthServer [authphrase]"

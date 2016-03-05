module GetFileCommon where

import Network.Simple.TCP (HostName)

type Port = String
type FileName = String

authHost = "127.0.0.1"
authPort = "8002"
filePort = "8003"

data AuthRequest = AuthRequest {
        username :: String,
        password :: String,
        fileHost :: HostName,
        dataRequest :: DataRequest
    } deriving (Show, Read)

data DataRequest = FileRequest {clientHost :: HostName, clientPort :: Port, fileNames :: [FilePath]} 
                 | ListRequest {clientHost :: HostName, clientPort :: Port}
                 deriving (Show, Read)

data DataResponse = FileResponse {files :: [String]}
                  | ListResponse {files :: [FileName]}
                  deriving (Show, Read)
import Network.Simple.TCP (connect, listen, accept, acceptFork, HostPreference(Host), HostName)
import Network.Socket (recv, send)
import Data.Foldable (traverse_)

portNum = "8001"

type ClassName = String
type PupilName = String
type Attendance = (ClassName, [PupilName])

listenForAttendance :: HostPreference -> IO ()
listenForAttendance hostPref = listen hostPref portNum $ \(socket, socketAddress) -> do
    acceptFork socket $ \(socket, remoteAddress) -> do
        str <- recv socket 4096
        let (className, pupilNames) = (read str :: (ClassName, [PupilName]))
        putStrLn $ "The pupils present in class " ++ className ++ " are:"
        traverse_ (\name -> putStrLn name) pupilNames
    listenForAttendance hostPref

main :: IO ()
main = listenForAttendance (Host "localhost")
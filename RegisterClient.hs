import Network.Simple.TCP (connect, listen, accept, HostPreference(Host), HostName)
import Network.Socket (recv, send)

portNum = "8001"

type ClassName = String
type PupilName = String
type Attendance = (ClassName, [PupilName])

readNames :: IO [PupilName]
readNames = do
    x <- getLine
    if x == "end" then
        return []
    else do
        rest <- readNames
        return $ x : rest

sendAttendance :: Attendance -> HostName -> IO Int
sendAttendance attendance host = do
    connect host portNum $ \(socket, remoteAddress) -> do
        send socket $ show attendance

main :: IO ()
main = do
    putStrLn "Which class is this?"
    className <- getLine
    putStrLn "Enter the present student names: ('end' to finish)"
    pupilNames <- readNames
    let hostName = "localhost"
    sendAttendance (className, pupilNames) hostName
    putStrLn $ "Sent attendance to '" ++ hostName ++ "'"

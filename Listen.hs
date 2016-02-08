import Migration
import System.Environment (getArgs)

main :: IO ()
main = do 
	args <- getArgs
	let port = head args
	listenForComp port

{-# LANGUAGE
    FlexibleContexts,
    ConstraintKinds,
    OverloadedStrings,
    OverloadedLists,
    RankNTypes #-}

import Migration
import System.Environment (getArgs)

checkPresence :: AbsList AbsString -> MigrationComp (AbsList AbsString)
checkPresence names = 
    let limited 0 _ = return Nil
        limited n xs = do
            x <- hd xs
            case x of Nothing -> return Nil
                      Just name -> do
                        printStr $ "Is " +++ name +++ " present?"
                        answer <- readStr
                        present <- answer === "y"
                        rest <- limited (n-1) (tl xs)
                        if present then return $ acons name rest
                                   else return rest
    in limited 3 names


registerComp :: MigrationComp ()
registerComp = do
    printStr "Which class is this?"
    className <- readStr
    printStr "Getting expected people..."
    --migrate ("127.0.0.1", "8002")
    --migrate ("127.0.0.1", "8001")
    presence <- checkPresence ["Oscar", "Jamie", "John"]
    printStr "Done. Reporting result."
    migrate ("127.0.0.1", "8002")
    printStr $ "The people present in class " +++ className +++ " are:"
    forEach (\name -> printStr name) presence
    return ()

main :: IO ()
main = do
    --args <- getArgs
    --let port = head args
    runMigrationComp "8001" registerComp



{-

Compile Haskell Case.hs

Run with all the numbers

Save .prof file for each number

remove *.o and *.hi

-}

import System.IO
import System.Process
import System.Exit
import System.Environment
import Control.Monad
import System.Info
import Data.List
import Control.Exception
import Data.Maybe
import Debug.Trace

configFile = "config.hd"

main = do 
    file <- fromArgs
    let pureFileName = reverse . fromJust . stripPrefix "sh." . reverse $ file
    
    putStrLn $ "Compiling " ++ file ++ " ..."
    ret <- system $ "ghc -O2 --make " ++ file ++ " -prof -auto-all -caf-all -fforce-recomp -rtsopts -threaded"
    case ret of
        e@(ExitFailure reason) -> print reason >> exitWith e
        _ -> return ()
    
    putStrLn $ "Removing temp files..."
    deleteFile "*.o *.hi"
    
    putStrLn $ "Reading " ++ configFile ++ " ..."
    sNum <- fmap read $ readFile configFile
    let (args,sargs) = ([0..sNum], map show args)
    evaluate ([0..sNum] :: [Int])
    
    forM_ sargs $ \n -> do
        -- run with all the numbers
        runFile pureFileName [n, "+RTS", "-p", "-hc","-K100M"]
        -- save .prof file
        renameFile ".prof" pureFileName n
        renameFile ".hp" pureFileName n
        system $ "hp2ps -e8in -c -i4 " ++ pureFileName ++ n ++  ".hp"
;

runFile :: FilePath -> [String] -> IO ()
runFile filename args = 
        void $ putStrLn ("shell> " ++ cmd) >> system cmd
            where
                cmd = case os of
                    "mingw32"   -> filename ++ ".exe" ++ args'
                    _           -> filename ++ args'
                args' = " " ++ intercalate " " args
;


-- rename File so that we have its copy.
-- also delete files from earlier
renameFile :: String -> FilePath -> String -> IO ExitCode
renameFile ending fp n =
        case os of
                "mingw32"   -> deleteFile new >> system ("rename " ++ args)
                _   -> deleteFile new >> system ("mv " ++ args)
            where
                args = old ++ " " ++ new
                old = fp ++ ending
                new = fp ++ n ++ ending
;

deleteFile :: String -> IO ExitCode
deleteFile args = case os of
                "mingw32"   -> system $ "del " ++ args  -- windows....
                _           -> system $ "rm -f " ++ args        -- unix? I dont know how Info.os looks on unix

fromArgs :: IO FilePath
fromArgs = (\xs -> case xs of x:_ -> x; _ -> "Case.hs") `liftM` getArgs

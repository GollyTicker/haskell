


import Network
import System.Environment
import System.IO
import Control.Concurrent
import GHC.Conc.Sync
import Control.Exception

import Data.Char (toLower, toUpper)
import Control.Monad (unless, void, when)
import Data.List
import Data.Maybe

import NetworkUtils

main = withSocketsDo $ do
    -- read port if given
    args <- getArgs
    let port = fromArgs args
    
    -- create socket
    socket <- listenOn port
    
    -- create shutdownMsg container
    mvar <- newEmptyMVar
    
    -- create threadList container
    tlsMvar <- newEmptyMVar
    
    putStrLn "Accepting Clients...."
    
    forkerID <- forkFinally (forker socket mvar)
                (\out -> case out of
                    Right res -> putStrLn ("Forker got Threads: " ++ show res) >> putMVar tlsMvar res
                    Left exp -> putStrLn "Exception: " >> print exp)

    -- until there was no shutdown, keep accepting and forking
    while (isEmptyMVar mvar) yield
    
    putStrLn "Killing forker."
    throwTo forkerID UserInterrupt
    tls <- takeMVar tlsMvar -- blocks until forker is finished
    
    putStrLn "Waiting for Clients to finish."
    while (existsM isRunning tls) yield
;


forker :: Socket -> MVar () -> IO [ThreadId]
forker socket mvar = let go = forker socket mvar
                      in
                        do
                            ids <- forkOrBreak socket mvar
                            case ids of
                                [] -> return []
                                id:_ -> fmap (id:) go
;
forkOrBreak :: Socket -> MVar () -> IO [ThreadId]
forkOrBreak socket mvar = (fmap (:[]) $ acceptAndFork socket mvar)
                          `catch` ( (const $ return []) :: AsyncException -> IO [ThreadId] )

acceptAndFork :: Socket -> MVar () -> IO ThreadId
acceptAndFork socket mvar = do
    newConnection <- accept socket
    forkIO (handleNewClient newConnection mvar)
;

isRunning :: ThreadId -> IO Bool
isRunning id = do stat <- threadStatus id
                  return $ case stat of 
                    ThreadRunning -> True
                    ThreadFinished -> False
                    ThreadBlocked reason -> True
                    ThreadDied -> False
;

handleNewClient (door, hostname, port) mvar = do
    sendLinewise door
    putStrLn $ "New Client: " ++ show door ++ ", " ++ show hostname ++ ", " ++ show port
    repl door mvar
    putStrLn $ "Quit Client:" ++ show door
;


repl :: Handle -> MVar ()-> IO ()
repl door mvar = 
    let receive = hGetLine door
        send = hPutStrLn door
        shutdownMsg = tryPutMVar mvar () >> putStrLn "Received Shutdown"
        loop = do
                -- receive a line
                recv <- receive
                showRecv recv
                let tokens = tokenize recv
                
                let toSend = process tokens
                send toSend
                showSent toSend
                
                -- send shutdown to the main thread if the server is to be shutdown
                when (isShutdown tokens) shutdownMsg
                
                -- recursive call
                unless (isBYE tokens || isShutdown tokens) loop
    in loop
;

responders :: [Tokens -> Maybe String]
responders = [
                \x -> case x of ["LOWERCASE",strs] -> return $ map toLower strs; _ -> Nothing,
                \x -> case x of ["UPPERCASE",strs] -> return $ map toUpper strs; _ -> Nothing,
                \x -> case x of ["REVERSE",strs] -> return $ reverse strs; _ -> Nothing,
                \x -> case x of ["SHUTDOWN"] -> return "SHUTDOWN"; _ -> Nothing,
                \x -> case x of ["BYE"] -> return "BYE"; _ -> Nothing,
                \x -> return $ "Unknown Command: " ++ showts x
             ]
;


process :: Tokens -> String
process tokens = head . catMaybes $ map ($ tokens) responders

isBYE :: Tokens -> Bool
isBYE ["BYE"] = True
isBYE _ = False

isShutdown :: Tokens -> Bool
isShutdown ["SHUTDOWN"] = True
isShutdown _ = False

showts :: Tokens -> String
showts ts = intercalate " " ts

type Tokens = [String]

tokenize :: String -> Tokens
tokenize = filter (not . (' ' `elem`))                  -- ["Ok","sdfs","sdf","end!"]
            . groupBy (\a b -> a /= ' ' && b /= ' ')    -- ["Ok"," ","sdfs"," ","sdf"," "," ","end!"," "] 
                                                        -- "Ok sdfs sdf  end! "

fromArgs :: [String] -> PortID
fromArgs args = parsePort $ case args of
                    portStr:_ -> portStr
                    [] -> defaultPort
;


    


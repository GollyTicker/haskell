


import Network
import System.Environment
import System.IO
import Control.Concurrent
import GHC.Conc.Sync
import Data.Char (toLower)
import Control.Monad (unless, void)

import NetworkUtils

main = withSocketsDo $ do
    -- read port if given
    args <- getArgs
    let port = fromArgs args
    
    -- create socket
    socket <- listenOn port
    
    while (return True) $ acceptAndFork socket
;

acceptAndFork :: Socket -> IO ()
acceptAndFork socket = do
    newConnection <- accept socket
    void $ forkIO (handleNewClient newConnection)
;

isRunning :: ThreadId -> IO Bool
isRunning id = do stat <- threadStatus id
                  return $ case stat of 
                    ThreadRunning -> True
                    ThreadFinished -> False
                    ThreadBlocked reason -> True
                    ThreadDied -> False
;

handleNewClient (door, hostname, port) = do
    sendLinewise door
    putStrLn $ "New Client: " ++ show door ++ ", " ++ show hostname ++ ", " ++ show port
    repl door
    putStrLn "Quit."
;


repl :: Handle -> IO ()
repl door = 
    let receive = hGetLine door
        send = hPutStrLn door
    in do
        -- receive a line
        recv <- receive
        showRecv recv
        
        let toSend = recv
        send toSend
        showSent toSend
        
        -- recursive call
        unless (map toLower recv == "bye") (repl door)
;



fromArgs :: [String] -> PortID
fromArgs args = parsePort $ case args of
                    portStr:_ -> portStr
                    [] -> defaultPort
;


    


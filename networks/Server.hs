


import Network
import System.Environment
import System.IO
import Control.Concurrent
import GHC.Conc.Sync

import NetworkUtils

-- ghc --make -threaded Server.hs


main = withSocketsDo $ do
    -- read port if given
    args <- getArgs
    let port = fromArgs args
    
    -- create socket
    socket <- listenOn port
    
    
    -- accept a connection
    newConnection <- accept socket
    threadID <- forkIO (handleNewClient newConnection)
    
    while (isRunning threadID) yield
    
    sClose socket
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
    let receive = hGetLine door
    let send = hPutStrLn door
    sendLinewise door
    
    -- receive a line
    recv <- receive
    showRecv recv
    
    -- send a response
    let toSend = recv
    send toSend
    showSent toSend
;


fromArgs :: [String] -> PortID
fromArgs args = parsePort $ case args of
                    portStr:_ -> portStr
                    [] -> defaultPort
;


    


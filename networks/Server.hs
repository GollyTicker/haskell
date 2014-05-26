


import Network
import System.Environment
import System.IO
import Control.Concurrent

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
    forkIO (handleNewClient newConnection)
    
    threadDelay $ 5*1000*1000
    
    sClose socket
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


    


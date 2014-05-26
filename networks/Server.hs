


import Network
import System.Environment
import System.IO

import NetworkUtils


main = withSocketsDo $ do
    -- read port if given
    args <- getArgs
    let port = fromArgs args
    
    -- create socket
    socket <- listenOn port
    -- accect a connection
    (door, hostname, port) <- accept socket
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
    
    sClose socket
;

fromArgs :: [String] -> PortID
fromArgs args = parsePort $ case args of
                    portStr:_ -> portStr
                    [] -> defaultPort
;


    


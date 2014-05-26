


import Network
import System.Environment
import System.IO

import NetworkUtils (parsePort, sendLinewise)


main = withSocketsDo $ do
    portStr:_  <- getArgs
    let port = parsePort portStr
    socket <- listenOn port
    (door, hostname, port) <- accept socket
    let receive = hGetLine door
    let send = hPutStrLn door
    sendLinewise door
    
    recv <- receive
    putStrLn $ "Recv: " ++ recv
    
    let toSend = recv
    send toSend
    putStrLn $ "Sent: " ++ toSend
    
    sClose socket
;
    


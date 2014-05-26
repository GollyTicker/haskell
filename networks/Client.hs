
import Network
import System.Environment
import System.IO

import NetworkUtils (parsePort, sendLinewise)


main = do
    server:portStr:_  <- getArgs
    let port = parsePort portStr
    -- door :: Handle
    door <- connectTo server port
    let receive = hGetLine door
    let send = hPutStrLn door
    sendLinewise door
    
    line <- getLine
    send line
    
    recv <- receive
    putStrLn $ "Got: " ++ recv
    
    hClose door
;


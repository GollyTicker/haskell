
import Network
import System.Environment
import System.IO

import NetworkUtils

-- ghc --make -threaded Client.hs

main = do
    -- read port and server
    args <- getArgs
    let (server, port) = fromArgs args
    
    -- create connection
    -- door :: Handle
    door <- connectTo server port
    let receive = hGetLine door
    let send = hPutStrLn door
    sendLinewise door
    
    -- send a line
    putStrLn "Write something..."
    line <- getLine
    send line
    showSent line
    
    -- receive a line
    recv <- receive
    showRecv recv
    
    hClose door
;


fromArgs :: [String] -> (HostName, PortID)
fromArgs args = fmap parsePort $ case args of
                    server:portStr:_ -> (server, portStr)
                    [server] -> (server, defaultPort)
                    [] -> ("localhost", defaultPort)
;

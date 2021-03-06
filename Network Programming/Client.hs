
import Network
import System.Environment
import System.IO

import Data.Char (toLower)
import Control.Monad (unless)

import NetworkUtils

main = do
    -- read port and server
    args <- getArgs
    let (server, port) = fromArgs args
    
    -- create connection
    door <- connectTo server port
    sendLinewise door
    
    repl door
    putStrLn "Quit."
    
    hClose door
;

repl :: Handle -> IO ()
repl door = 
    let receive = hGetLine door
        send = hPutStrLn door
    in do
        -- read a line
        putStrLn "Write a line: Quit with \"BYE\" or \"SHUTDOWN\""
        line <- getLine
        
        -- send a line
        send line
        
        -- receive a line
        recv <- receive
        showRecv recv
        
        unless (line == "BYE" || line == "SHUTDOWN") $ (repl door)
;

fromArgs :: [String] -> (HostName, PortID)
fromArgs args = fmap parsePort $ case args of
                    server:portStr:_ -> (server, portStr)
                    [server] -> (server, defaultPort)
                    [] -> ("localhost", defaultPort)
;




import Network
import System.Environment
import System.IO
import Control.Concurrent
import GHC.Conc.Sync

import Data.Char (toLower, toUpper)
import Control.Monad (unless, void)
import Data.List
import Data.Maybe

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
        let tokens = tokenize recv
        
        let toSend = process tokens
        send toSend
        showSent toSend
        
        -- recursive call
        unless (isBYE tokens) (repl door)
;

responders :: [Tokens -> Maybe String]
responders = [
                \x -> case x of ["LOWERCASE",strs] -> return $ map toLower strs; _ -> Nothing,
                \x -> case x of ["UPPERCASE",strs] -> return $ map toUpper strs; _ -> Nothing,
                \x -> case x of ["BYE"] -> return "BYE"; _ -> Nothing,
                \x -> return $ "Unrecognized: " ++ showts x
             ]
;

process :: Tokens -> String
process tokens = head . catMaybes $ map ($ tokens) responders

isBYE :: Tokens -> Bool
isBYE ["BYE"] = True
isBYE _ = False

showts :: Tokens -> String
showts ts = intercalate " " ts

type Tokens = [String]

tokenize :: String -> Tokens
tokenize = filter (not . (' ' `elem`))    -- ["Ok","sdfs","sdf","end!"]
            . groupBy (\a b -> a /= ' ' && b /= ' ')    -- ["Ok"," ","sdfs"," ","sdf"," "," ","end!"," "] 
            -- ["Ok","sdfs","sdf","end!"]

fromArgs :: [String] -> PortID
fromArgs args = parsePort $ case args of
                    portStr:_ -> portStr
                    [] -> defaultPort
;


    


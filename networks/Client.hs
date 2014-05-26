
import Network
import System.Environment
import System.IO
import NetworkUtils (parsePort)


main = do
    server:portStr:_  <- getArgs
    let port = parsePort portStr
    -- door :: Handle
    door <- connectTo server port
    hSetBuffering door LineBuffering    -- commands should be sent by Line
    getLine >>= hPutStrLn door
    hClose door
;


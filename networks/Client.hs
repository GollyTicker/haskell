
import Network
import System.Environment
import System.IO


main = do
    (server:portStr:_)  <- getArgs
    let port = parsePort portStr
    -- door :: Handle
    door <- connectTo server port
    getLine >>= hPutStrLn door
    hClose door
;


parsePort :: String -> PortID
parsePort = PortNumber . fromIntegral . read
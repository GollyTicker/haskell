

module NetworkUtils
        (
            parsePort,
            sendLinewise,
            defaultPort
        )
    where
;

import Network
import System.IO
    
parsePort :: String -> PortID
parsePort = PortNumber . fromIntegral . read

-- commands should be sent by Line
sendLinewise :: Handle -> IO ()
sendLinewise handle = hSetBuffering handle LineBuffering

defaultPort :: String
defaultPort = "50000"


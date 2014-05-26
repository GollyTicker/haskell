

module NetworkUtils
        (
            parsePort,
            sendLinewise
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


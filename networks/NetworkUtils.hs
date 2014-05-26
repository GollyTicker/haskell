

module NetworkUtils
        (
            parsePort,
            sendLinewise,
            defaultPort,
            showSent,
            showRecv
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

showRecv recv = putStrLn $ "<- " ++ recv
showSent sent = putStrLn $ "-> " ++ sent


defaultPort :: String
defaultPort = "50000"


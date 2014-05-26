

module NetworkUtils
        (
            parsePort,
            sendLinewise,
            defaultPort,
            showSent,
            showRecv,
            while
        )
    where
;

import Network
import System.IO

while :: Monad m => m Bool -> m () -> m ()
while holds f = let go = while holds f
                in holds >>= \b -> 
                   if b then f >> go else return ()
;
    
parsePort :: String -> PortID
parsePort = PortNumber . fromIntegral . read

-- commands should be sent by Line
sendLinewise :: Handle -> IO ()
sendLinewise handle = hSetBuffering handle LineBuffering

showRecv recv = putStrLn $ "<- " ++ recv
showSent sent = putStrLn $ "-> " ++ sent


defaultPort :: String
defaultPort = "50000"


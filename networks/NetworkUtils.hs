

module NetworkUtils
        (
            parsePort
        )
    where
;

import Network
    
parsePort :: String -> PortID
parsePort = PortNumber . fromIntegral . read


module NetworkUtils
        (
            parsePort,
            sendLinewise,
            defaultPort,
            showSent,
            showRecv,
            while,
            allM
        )
    where
;

import Network
import System.IO
import Control.Monad

-- StackExchange Haskell (f .) .g 
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

while :: Monad m => m Bool -> m () -> m ()
while holds f = let go = while holds f
                in holds >>= \b -> 
                   if b then f >> go else return ()
;

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f = foldM (\a b -> f b >>= (\b -> return (a && b))) True

parsePort :: String -> PortID
parsePort = PortNumber . fromIntegral . read

-- commands should be sent by Line
sendLinewise :: Handle -> IO ()
sendLinewise handle = hSetBuffering handle LineBuffering

showRecv recv = putStrLn $ "<- " ++ recv
showSent sent = putStrLn $ "-> " ++ sent


defaultPort :: String
defaultPort = "50000"


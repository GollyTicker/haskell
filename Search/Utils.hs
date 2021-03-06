
module Utils (
        module Utils
    )
    where

import Types

import Data.List
import Control.Applicative
import Control.Monad
import Text.Printf

printSolutions :: Problem p m a -> [Solution a] -> IO ()
printSolutions pr xs =
    printf "Showing %d solutions:\n" (length xs)
    *> (mapM_ (*> putStrLn "") . map (printSolution pr) ) xs

printSolution :: Problem p m a -> Solution a -> IO ()
printSolution pr = putStrLn . toString . zipWith f [0..] . reverse
    where
        f i (Node x aa _ _) = ( (show i ++ ": " ++ showAA aa), " -> ", pr `showElem` x )
        showAA Start = "Start"
        showAA (AA _ s n _) = s ++ " " ++ show n
        toString :: [(String,String,String)] -> String
        toString xs =
            let ds = map (\(x,_,_) -> length x) xs
                dMax  = 2 + maximum ds
            in  intercalate "\n" $ zipWith (\(a,b,c) d -> a ++ replicate (dMax-d) ' ' ++ b ++ c) xs ds


-- could be useful for printing and intercalating

-- generalizes intercalate on an applicative.
-- exeucte all items in xs in sequence with intermediate a's.
intercalateA :: Applicative f => f () -> [f ()] -> f ()
intercalateA a []     = pure ()
intercalateA a (x:[]) = x
intercalateA a (x:xs) = x *> a *> intercalateA a xs

-- *> for applicative is equivalent to >> for monad



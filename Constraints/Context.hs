{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Context (
        module Context
    )
    where

import Types
import Solving

import Control.Monad.Trans
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Map as M
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Identity

solveIO :: Net -> Config -> IO [Solution]
solveIO = solveGeneric (undefined :: R.ReaderT Config IO ())

-- solve takes a net and returns a list of solutions
-- uses ac3 with Full Lookahead
solve :: Net -> Config -> ([Solution], Int, Int, String)
solve = solveGeneric (undefined :: RWS Config String (Int, Int) ())

instance Context (R.ReaderT Config IO) Config (IO [Solution]) where
    countAC = return () -- no counting in IO
    countInference = return ()
    info x = do cfg <- R.ask
                when (verbose cfg)
                    $ lift $ putStrLn x
    ac net = do cfg <- R.ask
                algorithm cfg net
    runSolver cfg ma = R.runReaderT ma cfg


instance Context Solver Config ([Solution], Int, Int, String) where
    countAC =
        do cfg <- ask
           when (countStats cfg)
               $ modify ( \(a,b) -> (succ a,b) )
                    
    countInference =
        do cfg <- ask
           when (countStats cfg)
               $ modify ( \(a,b) -> (a,succ b) )

    info x = do cfg <- ask
                when (verbose cfg)
                    $ tell (x ++ "\n")

    ac net = do cfg <- ask
                algorithm cfg net
    
    runSolver cfg solver =
        let (r, (acs, infs), log) = runRWS solver cfg (0,0)
        in  (r, acs, infs, log)



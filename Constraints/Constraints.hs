
module Constraints (
        Net(..), Constraint, Node(..),
        Domain, NodeName, nodeName,
        mkConstraint, applyUnaryConstraint, findNode, var,
        On, Over,
        ac3,
        solve, solveIO, solveGeneric, expandFirstNode,
        defaultConfig,
        Context, countAC, countInference, info, ac,
        Solver, Config(..),
        Typeable, E(..)
    ) where

import Types
import ArcConsistency
import Solving

import Data.Typeable


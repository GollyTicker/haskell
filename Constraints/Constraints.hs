
module Constraints (
        Net(..), Constraint, Node(..),
        Domain, NodeName, nodeName,
        mkConstraint, applyUnaryConstraint, findNode, var,
        On, Over,
        ac3, solve,
        Typeable, E(..)
    ) where

import Types
import ArcConsistency
import Solving

import Data.Typeable


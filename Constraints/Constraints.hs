
module Constraints (
        Net(..), Constraint, Node,
        Domain, NodeName,
        mkConstraint, var,
        ac3, solve
    ) where

import Types
import ArcConsistency
import Solving


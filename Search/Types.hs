{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
module Types (
        module Types
    )
    where

import qualified Data.Set as S
import Data.Monoid
import Data.Maybe
import Control.Monad

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = \x y -> f $ g x y

data Strategy a =
    Depth
    | Breadth
    | A
--  | OptimistcClimbing
--  | Custom (StrategyF a)

type StrategyF p a = [p a] -> [p a] -> [p a]
--                 NewPaths -> OldPaths -> AllPaths

-- Problem a is a description of a problem where the search space
-- consists of elemets of type a.
-- In example, the water bucket problem might use (Int, Int)
-- where the two numbers represent the liters in each bucket.
data Problem p m a =
    Problem {
         starts         :: [a]
        ,showElem       :: a -> String  -- keep (show) in problem description
        ,checkGoal      :: a -> m Bool
        ,eqElem         :: a -> a -> Bool
        ,heuristic      :: Maybe (Heuristic a)
        ,actions        :: [Action m a]
        ,strategy       :: Strategy a
        ,ordering       :: Maybe (a -> a -> Ordering) -- if given, allows usage of Sets to optimize cycle detection
        ,noCycleDetection :: Bool
            -- enable, if cycles are impossible in your domain.
            -- reduces time comsumption
        -- add custom strategies here?
        
        ,_hidden         :: Maybe (m a, p a) -- needed to make sure,
                                        -- GHC interprets
                                        -- phantom type p :: * -> *
    }

-- default Problem for easier use with record Syntax
mkProblem :: Problem p m a
mkProblem = Problem {
         starts = error "mkProblem{starts = ... Please specify ... } "
        ,showElem = error "mkProblem{showElem = ... Please specify ... } "
        ,checkGoal = error "mkProblem{checkGoal = ... Please specify ... } "
        ,eqElem = error "mkProblem{eqElem = ... Please specify ... } "
        ,heuristic = Nothing
        ,actions = error "mkProblem{actions = ... Please specify ... } "
        ,strategy = Breadth
        ,noCycleDetection = False
        ,ordering = Nothing
        ,_hidden = Nothing
    }

checkGoalNode :: Problem p m a -> Node a -> m Bool
checkGoalNode pr = (pr `checkGoal`) . getElem

data Node a =
    Node {
         getElem        :: a
        ,getAction      :: AppliedAction a
        ,getHvalue      :: Maybe HValue   -- Heuristics Value
        ,ord            :: a -> a -> Ordering
    }

instance Ord (Node a) where
    compare (Node a _ _ f) (Node b _ _ _) = f a b

instance Eq (Node a) where (==) = (EQ==) .: compare

toNode :: Node a -> AppliedAction a -> Node a
toNode pred aa@(AA x _ _ _) = Node x aa Nothing (ord pred)

mkAction :: forall m a. Monad m => String -> (a -> m [a]) -> Action m a
mkAction s f = Action s h --
    where
        h :: Node a -> m [AppliedAction a]
        h node@(Node x _ _ _) =
                    liftM
                        (zipWith (g node) [0..])
                        (f x)
        g node n y = AA y s n node

data Action m a = Action String (Node a -> m [AppliedAction a])

data AppliedAction a =
    Start
    | AA a String Int (Node a)
   -- result, name, variationID, predecessor
   -- (the states it has been applied to)

applyOn :: Node a -> Action m a -> m [AppliedAction a]
applyOn a (Action _ f) = f a

-- user-friendly
getPath :: Solution a -> [a]
getPath = map getElem

-- a Path is a list of Set of Nodes or a simple list of nodes.
-- in the set version,the order comes from the backward references in the AAs in the nodes.
-- the first node is saved in the set version
data    SPath a = SPath { __head :: Node a, getSet :: S.Set (Node a) }
newtype LPath a = LPath { getLPath :: [Node a] }


class PathT p a where
    first        :: p a -> Node a
    prepend      :: Node a -> p a -> p a
    mkStartPath  :: Problem p m a -> a -> p a
    toSolution   :: p a -> Solution a
    evalPathWith :: PathT p a => (a -> HValue -> HValue) -> p a -> p a
    -- TODO: evalPathWith - alte Justs nicht erneut berechnen
    contains     :: Problem p m a -> p a -> Node a -> Bool

instance PathT SPath a where
    first (SPath x _) = x
    prepend x s = SPath x . S.insert x . getSet $ s
    mkStartPath pr x =
        case ordering pr of
            Nothing -> missingOrderingError
            Just f -> let node = Node x Start Nothing f
                      in  SPath node $ S.singleton (node)

    toSolution s = toPath . first $ s
        where
            toPath :: Node a -> [Node a]
            toPath n = n:case getAction n of
                            Start -> []
                            AA _ _ _ n' -> toPath n'

    evalPathWith f s@(SPath n xs)
        | evaled s = s
        | otherwise = 
            let hvalue = f (getElem n) (fromIntegral (S.size xs - 1))
                new = n { getHvalue = Just hvalue }
            in  SPath new (replace n new xs)
        
    contains pr ns n = case ordering pr of
                        Nothing -> missingOrderingError
                        Just _  -> S.member n (getSet ns)

evaled :: PathT p a => p a -> Bool
evaled = isJust . getHvalue . first

missingOrderingError = error "Set based approach requires mkProblem{ordering = ...}"

replace :: Ord a => a -> a -> S.Set a -> S.Set a
replace old new = S.insert new . S.delete old

instance PathT LPath a where
    first (LPath (n:_)) = n
    prepend n p = LPath $ n: getLPath p
    mkStartPath pr x = LPath [Node x Start Nothing err]
        where
            err = error $ "This is a bug. Ordering in Node should not be accessed in (PathT Path a)"

    toSolution = getLPath
    evalPathWith f p@(LPath (node:ns) )
        | evaled p = p
        | otherwise = 
            let hvalue = f (getElem node) (fromIntegral (length ns))
            in  LPath (node {getHvalue = Just hvalue} :ns)
    contains pr ns n = any (\n' -> (getElem n) `eq` (getElem n')) . getLPath $ ns
        where eq = eqElem pr

type Heuristic a = a -> HValue

type HValue = Double -- currently, limiting heuristics to Double

type Solution a = [Node a]

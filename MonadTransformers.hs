
-- My run at Monad Transformers

-- Tutorial: http://www.cs.virginia.edu/~wh5a/personal/Transformers.pdf

import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Text.Printf
import Data.Time

-- run all the evals with runs !! 0, runs !! 1, ...


type Name = String

data Exp =
        Lit Integer
        | Var Name
        | Plus Exp Exp
        | Abs Name Exp
        | App Exp Exp
        deriving Show

data Value = IntVal Integer | FunVal Env Name Exp
            deriving Show

type Env = Map.Map Name Value


-- straight-forward implementation
-- done without looking at PDF
-- examples by Swaneet *1

eval0 :: Env -> Exp -> Value
eval0 env (Lit n)       = IntVal n
eval0 env (Var x)       = env Map.! x
eval0 env (Plus x y)    = case (eval0 env x, eval0 env y) of
                            (IntVal x', IntVal y') -> IntVal (x' + y')
eval0 env (Abs x ex)     = FunVal env x ex
eval0 env (App ex1 ex2) = let   (FunVal envF x exI) = eval0 env ex1
                                arg = eval0 env ex2
                                newEnv = Map.insert x arg envF 
                          in eval0 newEnv exI


-- 12 + ((\x -> x) (4 + 2))
exampleExp = Lit 12 `Plus` ((Abs "x" (Var "x")) `App` (Lit 4 `Plus` Lit 2))


runEval0 = eval0

-- Monad Transformers

type Eval1 a = Identity a
runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

-- could be generally for Monad m => m Value
eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit n)    = return $ IntVal n
eval1 env (Var x)       = return $ env Map.! x -- Map.lookup x env-- autocalls Identity's fail function
eval1 env (Plus e1 e2)  = do
                            IntVal x' <- eval1 env e1
                            IntVal y' <- eval1 env e2
                            return $ IntVal (x' + y')
eval1 env (Abs x e)     = return $ FunVal env x e
eval1 env (App e1 e2)   = do
                            (FunVal envF x eI) <- eval1 env e1
                            arg <- eval1 env e2
                            let newEnv = Map.insert x arg envF
                            eval1 newEnv eI


-- Evaluation with informative failures

type Eval2 a = ErrorT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 env = runIdentity (runErrorT env)

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit n)    = return $ IntVal n
eval2 env (Var x)       = case Map.lookup x env of
                            Nothing -> throwError $ "Unbound " ++ x ++ " in " ++ show env
                            Just v  -> return v
eval2 env (Plus e1 e2)  = do
                            val1 <- eval2 env e1
                            val2 <- eval2 env e2
                            case (val1, val2) of
                                (IntVal x', IntVal y') -> return $ IntVal (x' + y')
                                _ -> throwError $ printf "Type mismatch: Expected IntVal for %s and %s" (show e1) (show e2)
eval2 env (Abs x e)     = return $ FunVal env x e
eval2 env (App e1 e2)   =
    do
        fVal <- eval2 env e1
        case fVal of
            (FunVal envF x eI) ->
                eval2 env e2 >>= \arg -> 
                eval2 (Map.insert x arg envF) eI
            _ -> throwError $ printf "Type mismatch: Expedted FunVal for %s" (show fVal)


-- Evaluation with added environment reading

type Eval3 a = ReaderT Env (ErrorT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env eval = runIdentity (runErrorT (runReaderT eval env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit n)   = return $ IntVal n
eval3 (Var x)   = ask >>= \env -> 
                    case Map.lookup x env of -- *2
                        Nothing -> throwError $ "Unbound " ++ x ++ " in " ++ show env
                        Just v  -> return v
eval3 (Plus e1 e2)  =
        do
           val1 <- eval3 e1
           val2 <- eval3 e2
           case (val1, val2) of
               (IntVal x', IntVal y') -> return $ IntVal (x' + y')
               _ -> throwError $ printf "Type mismatch: Expected IntVal for %s and %s" (show e1) (show e2)
eval3 (Abs x e)     = ask >>= \env -> 
                        return $ FunVal env x e
eval3 (App e1 e2)   =
    do
        fVal <- eval3 e1
        case fVal of
            (FunVal envF x eI) ->
                eval3 e2 >>= \arg -> 
                local (const $ Map.insert x arg envF) (eval3 eI)
            _ -> throwError $ printf "Type mismatch: Expedted FunVal for %s" (show fVal)


-- Evaluation with added counting evaluation steps

type Eval4 a =
    ReaderT Env (
        ErrorT String (
            StateT Int (
                Identity 
            )
        )
    ) a

runEval4 :: Env -> Int -> Eval4 a -> (Either String a, Int)
runEval4 env s eval =
    runIdentity (
        runStateT (
            runErrorT (
                runReaderT eval env
            )
        ) s
    )

tick :: (Num s, MonadState s m) => m ()
tick = modify (+1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit n)   = tick >>= \_ -> return $ IntVal n -- tick >> return (...) doesnt work?!
eval4 (Var x)   =
        do  tick
            env <- ask 
            case Map.lookup x env of
                Nothing -> throwError $ "Unbound " ++ x ++ " in " ++ show env
                Just v  -> return v
eval4 (Plus e1 e2)  =
        do tick
           val1 <- eval4 e1
           val2 <- eval4 e2
           case (val1, val2) of
               (IntVal x', IntVal y') -> return $ IntVal (x' + y')
               _ -> throwError $ printf "Type mismatch: Expected IntVal for %s and %s" (show e1) (show e2)
eval4 (Abs x e)     = 
        do  tick
            env <- ask 
            return $ FunVal env x e
eval4 (App e1 e2)   =
    do  tick
        fVal <- eval4 e1
        case fVal of
            (FunVal envF x eI) ->
                eval4 e2 >>= \arg -> 
                local (const $ Map.insert x arg envF) (eval4 eI)
            _ -> throwError $ printf "Type mismatch: Expedted FunVal for %s" (show fVal)


-- Evaluation with added logging of occurred variable bindings

type Eval5 a =
    ReaderT Env (
        ErrorT String (
            WriterT [String] (
                StateT Int (
                    Identity 
                )
            )
        )
    ) a

runEval5 :: Env -> Int -> Eval5 a -> ((Either String a, [String]), Int)
runEval5 env s eval =
    runIdentity (
        runStateT (
            runWriterT (
                runErrorT (
                    runReaderT eval env
                )
            )
        ) s
    )

eval5 :: Exp -> Eval5 Value
eval5 (Lit n)   = tick >>= \_ -> return $ IntVal n
eval5 (Var x)   =
        do  tick
            tell [x]
            env <- ask 
            case Map.lookup x env of
                Nothing -> throwError $ printf "Unbound %s in %s" (show x) (show env)
                Just v  -> return v
eval5 (Plus e1 e2)  =
        do tick
           val1 <- eval5 e1
           val2 <- eval5 e2
           case (val1, val2) of
               (IntVal x', IntVal y') -> return $ IntVal (x' + y')
               _ -> throwError $ printf "Type mismatch: Expected IntVal for %s and %s" (show e1) (show e2)
eval5 (Abs x e)     = 
        do  tick
            env <- ask 
            return $ FunVal env x e
eval5 (App e1 e2)   =
    do  tick
        fVal <- eval5 e1
        case fVal of
            (FunVal envF x eI) ->
                eval5 e2 >>= \arg -> 
                local (const $ Map.insert x arg envF) (eval5 eI)
            _ -> throwError $ printf "Type mismatch: Expedted FunVal for %s" (show fVal)



-- Evaluation with added IO !!

type Eval6 a =
    ReaderT Env (
        ErrorT String (
            WriterT [String] (
                StateT Int (
                    IO 
                )
            )
        )
    ) a

runEval6 :: Env -> Int -> Eval6 a -> IO ((Either String a, [String]), Int)
runEval6 env s eval =
        runStateT (
            runWriterT (
                runErrorT (
                    runReaderT eval env
                )
            )
        ) s

eval6 :: Exp -> Eval6 Value
eval6 (Lit n)   = tick >>= \_ -> return $ IntVal n
eval6 (Var x)   =
        do  tick
            tell [x]
            env <- ask 
            case Map.lookup x env of
                Nothing -> throwError $ printf "Unbound %s in %s" (show x) (show env)
                Just v  -> return v
eval6 (Plus e1 e2)  =
        do tick
           val1 <- eval6 e1
           val2 <- eval6 e2
           liftIO $ getCurrentTime >>= \t -> putStrLn $ printf " < DingDong, its %s now > " (show t)
           case (val1, val2) of
               (IntVal x', IntVal y') -> return $ IntVal (x' + y')
               _ -> throwError $ printf "Type mismatch: Expected IntVal for %s and %s" (show e1) (show e2)
eval6 (Abs x e)     = 
        do  tick
            env <- ask
            liftIO $ putStrLn "dis doesn't want to be evaluated"
            return $ FunVal env x e
eval6 (App e1 e2)   =
    do  tick
        fVal <- eval6 e1
        case fVal of
            (FunVal envF x eI) ->
                eval6 e2 >>= \arg -> 
                local (const $ Map.insert x arg envF) (eval6 eI)
            _ -> throwError $ printf "Type mismatch: Expedted FunVal for %s" (show fVal)


-- *1
-- Examples by Swaneet
n, p, myvar, myAbs, myAbs2, app, app2 :: Exp
n = Lit 15
myvar = Var "x"
p = Plus n n
myAbs = Abs "y" myvar  
myAbs2 = Abs "x" myvar 
app = App myAbs n
app2 = App myAbs2 n

env = Map.fromList [("y",IntVal 3),("x",IntVal 42)] :: Env
vals = [
    n,
    myvar,
    p,
    myAbs,
    myAbs2,
    app,
    app2,
    Var "x" `Plus` Var "y",
    exampleExp
    ]

fails = [
    Lit 15 `App` Lit 16,
    Var "z",
    myAbs `Plus` Var "x"
    ]


runAll, runFails :: (Show b) => (a -> b) -> (Env -> Exp -> a)-> IO ()
runAll  = runGen vals
runFails  = runGen fails

runGen :: (Show b) => [Exp] -> (a -> b) -> (Env -> Exp -> a)-> IO ()
runGen ls runF eval = mapM_ (print . runF . eval env) ls

runGenR :: (Show b) => [Exp] -> (Env -> a -> b) -> (Exp -> a)-> IO ()
runGenR ls runF eval = mapM_ (print . runF env . eval) ls

runGenRS :: (Show b) => [Exp] -> Int -> (Env -> Int -> a -> b) -> (Exp -> a)-> IO ()
runGenRS ls st runF eval = mapM_ (print . runF env st . eval) ls

runGenRWSIO :: (Show b) => [Exp] -> Int -> (Env -> Int -> a -> IO b) -> (Exp -> a)-> IO ()
runGenRWSIO ls st runF eval = mapM_ (\x -> runF env st (eval x) >>= print) ls

runs =
    [
        mapM_ (print . eval0 env) vals,
        runAll runEval1 eval1,
        runAll runEval2 eval2 >> runFails runEval2 eval2,
        runGenR vals runEval3 eval3 >> runGenR fails runEval3 eval3,
        runGenRS vals 0 runEval4 eval4 >> runGenRS fails 0 runEval4 eval4,
        runGenRS vals 0 runEval5 eval5 >> runGenRS fails 0 runEval5 eval5,
        runGenRWSIO vals 0 runEval6 eval6 >> runGenRWSIO fails 0 runEval6 eval6
    ]


-- *2
-- we don't need to lift throwError function from ReaderT
-- into ErrorT because the developers of this library already have done this!

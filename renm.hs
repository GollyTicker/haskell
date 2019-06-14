{-
    
    renm
  
  Rename filenames. Match parts of filenames and refer to
  the matched results directly when creating new filenames.
  
  Usage form:
            $ renm <filename matcher> <filename creator>
  
  Usage example:
        $ renm "abc[a|d+]def.txt" "bla[a]blubb.txt"
                    ^  ^               ^
                    |  |               |
     matched variable  |               refer to match via variable
                       |
                       pattern matcher
    
    Renames all files
      starting with "abc"
      with one or more digits called refered to by <a>
      and ending with "def.txt"
    into files starting with "bla"
      with <a> inbetween
      and ending with "blub.txt".
  
  Note:
    Searches and replaces only through the files in the current directory.
  
  TODO:
    . Illustrate with many concrete examples
    . Describe inner expression matcher language.
    . find concrete use cases and improve towards usability
-}

import Control.Monad
import Data.Monoid

import Pipes
import qualified Pipes.Prelude as P

import Text.Parsec
import Data.Functor.Identity

-- Tutorial: https://hackage.haskell.org/package/pipes-4.3.10/docs/Pipes-Tutorial.html

import System.Directory -- package directory
import System.Console.ArgParser hiding (Parser)-- package argparser

import qualified Data.Map  as M
import qualified Data.List as L
import Data.Either


{- === MAIN, Argument Parsing, User Interface === -}

main :: IO ()
main = withParseResult argParser prog

data A = A String String deriving (Show)
argParser :: ParserSpec A
argParser = A `parsedBy` reqPos "file names patten for names to be changed"
              `andBy` reqPos "resulting file names"

prog :: A -> IO ()
prog (A s1 s2) =
  case parseMatCrt s1 s2 of
    Right (mat,crt) -> do
      putStrLn $ concat ["Understood matcher <",s1,"> and creator <",s2,">."]
      xs <- listDirectory "."
      runEffect $ each xs >-> tryReplace mat crt
      -- Perhaps count renamed files and report back to user?
    Left err -> do
      putStrLn ("Is your " ++ (sourceName $ errorPos err) ++ " correctly formed?")
      putStrLn ""
      putStrLn $ indent 2 $ show err
      putStrLn "Nothing was renamed."
  where
    indent n = unlines . map ("  " ++ ) . lines

-- for ghci>
runMe = sequence_ $ L.intersperse (putStrLn "=====") progs 
progs = [
   prog (A "dryrun-bla.txt" "blubb.txt")
  ,prog (A "[a|a+].[b|a+]" "[a].[b]")
  ,prog (A "[a|a+]-[b|a+]" "[a]-[b]")
  ,prog (A "dryrun[a|bad-expression].txt" "blubb[a].txt")
  ,prog (A "dryrun[a|a][a|a]" "a")
  ,prog (A "dryrun[a|a]" "[b]")
  ,print $ parse matcher "1" "a[b|c]d" >>= ((\x -> parse x "2" "axd") . snd)
  ,print $ parse matcher "1" "a[b|c+]d" >>= ((\x -> parse x "2" "axxd") . snd)
  ,innerExprProg
  ]


{- ==== Processing, Matching and Renaming ==== -}

parseMatCrt :: String -> String -> Either ParseError (Matcher,Creator)
parseMatCrt s1 s2 = do
  mat@(vars,_) <- parse matcher ("matcher <" ++ s1 ++ ">") s1
  crt <- parse (creator vars) ("creator <" ++ s2 ++ ">") s2
  return (mat,crt)

tryReplace :: Matcher -> Creator -> Consumer String IO ()
tryReplace mat crt = forever $ do
  nm <- await
  every (fileExists nm)
    >-> matchTransform mat crt
    >-> renmFile

fileExists :: String -> ListT IO String
fileExists nm = do
  b <- lift $ doesFileExist nm
  Select $ when b (yield nm)

matchTransform :: Monad m => Matcher -> Creator -> Pipe String (String,String) m ()
matchTransform (_,p) (_,f) = do
  s <- await
  case parse p "" s of
    Right env -> yield (s,f env)
    Left x -> return () -- lift $ print x

renmFile :: Consumer (String,String) IO ()
renmFile = do
  (x,y) <- await
  lift $ putStrLn $ concat ["Replace ", x, " -> ", y]
-- TODO: actually replace


{- === Parse matcher and creator === -}
type Matcher = ([Var],Parser Env)
type Creator = ([Var],Env -> String)

type Parser a = ParsecT String () Identity a

type Env = M.Map Var String
type Var = String

-- CAN-DO: could get rid of Env here and directly return another
-- Parser which itself returns a String
matcher :: Parser Matcher
matcher = do
  int <- chainl
    (do s <- txt; return ([],string s >> return M.empty))
    (do (v,p) <- bodyInnerExp
        return
          (\lt rt -> -- Monoids are powerful!
            mconcat [lt,([v],p >>= (return . M.singleton v)),rt]
          )
    )
    ([],return M.empty)
  let res = (<* eof) <$> int  -- parser finishes on end of filename
  eof
  let nonUniqueVars = fst res L.\\ (L.nub (fst res))
  when (not . null $ nonUniqueVars)
    $ parserFail $ "Duplicate variable names "
             ++ L.intercalate "," nonUniqueVars
             ++ ". Please give each expression an unique name."
  return res

-- CAN-DO:
-- parse matcher "1" "a[b|c+]d" >>= ((\x -> parse x "2" "axd") . snd)
-- fails with unexpected "d".
-- => make parsing work with constraints from the end?
-- perhaps it is natural to expect,
-- that each internal expression ends with something that is different
-- than the following character
-- => we could check whether, after the inner expr.
--    comes a matching string. but this check isnt guaranteed to
--    hold for all cases ad a consecutive inner expression
--    could also be the matched!


creator :: [Var] -> Parser Creator
creator availVars = do
  x <- chainl
    ((\x -> ([],const x)) <$> txt)
    (do v <- bodyVar
        return (\lt rt -> mconcat [lt, ([v],\env -> env M.! v), rt]))
    ([],const "")
  eof
  
  let unknownVars = (L.nub $ fst x) L.\\ availVars
  when (not . null $ unknownVars)
    $ parserFail $ "Unknown variable names "
             ++ L.intercalate "," unknownVars
             ++ " in creator. Did you misspell a variable?"
  return x

txt :: Parser String
txt = many (noneOf "[")

bodyInnerExp :: Parser (Var,Prs)
bodyInnerExp =
  do char '['
     var <- many letter
     char '|'
     exp <- prs
     char ']'
     return (var,exp)

bodyVar :: Parser Var
bodyVar = between (string "[") (string "]") (many letter)


{- === Inner Expression Parser === -}
type Prs = Parser String
type PrsParser = Parser Prs
prs :: PrsParser
prs =
  let withLA :: PrsParser
      withLA  = atomic "d" digit
                <|> atomic "c" letter
                <|> atomic "a" alphaNum
                <|> between (string "(") (string ")") prs
      
      withMany :: PrsParser -> PrsParser
      withMany pp =
        do p <- pp
           choice [
             string "+" >> return (concat <$> many1 p)
            ,string "*" >> return (concat <$> many  p)
            ,return p
            ]
  
  in  do ps <- many (withMany withLA)
         return $ concat <$> sequence ps

atomic :: String -> Parser Char -> PrsParser
atomic s p = string s >> return ((:[]) <$> p)

-- .+
-- .*
-- d for digits
-- c for characters
-- a for alphanumeric
{-
exp = many* -- means zero or more times
many = base "+" | base "*"
base = "d" | "c" | "a" | "(" exp ")"

NEXT-DO: add useful capabilities like a real reg-exp parser does.
e.g. ranges like 0-9 and co.

TODO:
  . add espacing for [ and ]
-}

innerExprProg :: IO ()
innerExprProg = do
  let f e s = do
        putStrLn $ concat ["Running expr: ",e," on ",s]
        print $ testPrs e s 
      exp1 = "(c+d+)+"
      str1 = "ab12cd34ef56blabla423423"
      exp2 = "(cd*c)+"
      str2 = "a1ab2323be12312e"
  f exp1 str1
  f exp2 str2

testPrs prsexp prsstr =
  (\p -> parse p "inner" prsstr)
  <$> parse prs "outer" prsexp

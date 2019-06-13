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
                           pattern matched against
    
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

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text

import Text.Parsec
import Data.Functor.Identity

-- Tutorial: https://hackage.haskell.org/package/pipes-4.3.10/docs/Pipes-Tutorial.html

import System.Directory -- package directory
import System.Console.ArgParser hiding (Parser)-- package argparser

import qualified Data.Map as M
import qualified Data.List as L

data A = A String String deriving (Show)

argParser :: ParserSpec A
argParser = A `parsedBy` reqPos "file names patten for names to be changed"
              `andBy` reqPos "resulting file names"

renmFile :: Consumer (String,String) IO ()
renmFile = do
  (y,x) <- await
  lift $ putStrLn $ concat ["Replace ", x, " -> ", y]
-- TOOD: actually replace

fileExists :: String -> ListT IO String
fileExists nm = do
  b <- lift $ doesFileExist nm
  Select $ when b (yield nm)

replace :: Matcher -> Creator -> String -> (String,String)
replace pat repl str = (str,str)

matches :: Matcher -> String -> Bool
matches pat str = True

pipeline :: Matcher -> Creator -> Consumer String IO ()
pipeline pat repl = forever $ do
  nm <- await
  every (fileExists nm)
    >-> P.filter (matches pat)
    >-> P.map (replace pat repl)
    >-> renmFile
;
-- Perhaps count renamed files and report back to user?

parsedArgs :: String -> String -> Either ParseError (Matcher,Creator)
parsedArgs s1 s2 = 
         (,) <$> parse matcher ("matcher <" ++ s1 ++ ">") s1
             <*> parse creator ("creator <" ++ s2 ++ ">") s2

main :: IO ()
main = withParseResult argParser prog

prog :: A -> IO ()
prog (A s1 s2) =
  case parsedArgs s1 s2 of
    Right (pat,out) -> do
      -- TODO. check that all variables in creator are created in matcher
      putStrLn $ concat ["Understood matcher <",s1,"> and creator <",s2,">."]
      xs <- listDirectory "."
      runEffect $ each xs >-> pipeline pat out
    Left err -> do
      putStrLn ("Is your " ++ (sourceName $ errorPos err) ++ " correctly formed?")
      putStrLn ""
      putStrLn $ indent 2 $ show err
      putStrLn ""
      putStrLn "Nothing was done."

indent n = unlines . map ("  " ++ ) . lines

-- for ghci>
runProg1,runProg2 :: IO ()
runProg1 = prog (A "bla.txt" "blubb.txt")
runProg2 = prog (A "bla[a|bad-expression].txt" "blubb[a].txt")
runProg3 = prog (A "[a|a][a|a]" "a")


{- === Parse matcher and out-filenames === -}
type Parser a = ParsecT String () Identity a
type Var = String
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

type Env = M.Map Var String
type Creator = ([Var],Env -> String)
type Matcher = ([Var],Parser Env)

creator :: Parser Creator
creator =
  chainl
    ((\x -> ([],const x)) <$> txt)
    (do v <- bodyVar
        return (\lt rt -> mconcat [lt, ([v],\env -> env M.! v), rt]))
    ([],const "") -- TODO: is this line correct?

-- could get rid of Env here and directly return another
-- Parser which itself returns a String
matcher :: Parser Matcher
matcher = do
  x <- chainl
    (const ([],return M.empty) <$> txt)
    (do (v,p) <- bodyInnerExp
        return
          (\lt rt -> -- Could this be more concise using monoid and Alternative?
            (mconcat [fst lt,[v],fst rt], (\l s r -> mconcat [l,M.singleton v s,r]) <$> snd lt <*> p <*> snd rt)
          )
    )
    ([],return M.empty)
  let nonUniqueVars = fst x L.\\ (L.nub (fst x))
  when (not . null $ nonUniqueVars)
    $ parserFail $ "Duplicate variable names "
             ++ L.intercalate "," nonUniqueVars
             ++ ". Please give each expression an unique name."
  return x
-- Error condition: multiple variables may clash.
-- fixed by checking it after parsing.

testPrs prsexp prsstr =
  (\p -> parse p "inner" prsstr)
  <$> parse prs "outer" prsexp

tryMeOut :: IO ()
tryMeOut = do
  let f e s = do
        putStrLn $ concat ["Running expr. ",e," on ",s]
        print $ testPrs e s 
      exp1 = "(c+d+)+"
      str1 = "ab12cd34ef56blabla423423"
      exp2 = "(cd*c)+"
      str2 = "a1ab2323be12312e"
  f exp1 str1
  f exp2 str2



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

TODO: add useful capabilities like a real reg-exp parser does
-}

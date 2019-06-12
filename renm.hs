{-
    renm
  
  Rename filenames by pattern matching for a string and replacing it one by one.
  
  Usage example:
    $ renm "abc[a|d+]def.txt" "bla[a]blubb.txt"
    
    Renames all files starting with "abc", ending with "def.txt" with
    one of more digits inbetween (match called a) into files "bla" + a + "blub.txt".
    
  Note:
    Finds and searches only through the files 
  
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
import System.Console.ArgParser -- package argparser

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

replace :: String -> String -> String -> (String,String)
replace pat repl str = (str,str)

matches :: String -> String -> Bool
matches pat str = True

pipeline :: String -> String -> Consumer String IO ()
pipeline pat repl = forever $ do
  nm <- await
  every (fileExists nm)
    >-> P.filter (matches pat)
    >-> P.map (replace pat repl)
    >-> renmFile
;

main = do
  let prog (A s1 s2) = do
        xs <- listDirectory "."
        runEffect $ each xs >-> pipeline s1 s2
  {-withParseResult argParser-}
  prog (A "bla.txt" "blubb.txt")
;

txt :: ParsecT String u Identity String
txt = many (noneOf "[]")

body :: ParsecT String u Identity (String,Prs u)
body = do char '['
          var <- many (noneOf "|")
          char '|'
          rexp <- txt
          char ']'
          return (var,rexp)
;

type Prs u = String -- ParsecT String u Identity String
{-
type PrsParser u = ParsecT String u Identity (Prs u)


test prsexp prsstr = fmap (\p -> parse p "inner" prsstr) $ parse prs "outer" prsexp
  
prs :: PrsParser u
prs = prsAtomic 'd' digit
  <|> prsAtomic 'c' letter
  <|> prsAtomic 'a' alphaNum
  <|> prsMany1 prs
  <|> prsBracket prs

prsBracket :: PrsParser u -> PrsParser u
prsBracket pp = do
  char '('
  p <- pp
  char ')'
  return p

prsMany1 :: PrsParser u -> PrsParser u
prsMany1 pp =
  do char '+'
     p <- pp
     return (fmap concat $ many1 p)
;

prsAtomic :: Char -> ParsecT String u Identity Char -> PrsParser u
prsAtomic c p = do char c; return (fmap (:[]) p)

-- .+
-- .* (not implemented)
-- d for digits
-- c for characters
-- a for alphanumeric
{-
srexp =
    "d"
  | "c"
  | "a"
  | +srxp
  | "(" srexp ")"
  | srxp srxp -- NOT IMPLEMENTED. TODO
-}

-}
bla :: ParsecT String u Identity [Either String (String,Prs u)]
bla = chainl
  (fmap ((:[]) . Left) txt)
  (do cont <- body;
      return (\xs1 xs2 -> xs1 ++ [Right cont] ++ xs2))
  [Left "noOcc"]

trythisout = parseTest bla "asda[s|asdsa]ds[ads|esrfdsd]asd"
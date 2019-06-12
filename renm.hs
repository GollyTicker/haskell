{-
    renm
  
  Rename filenames by pattern matching for a string and replacing it one by one.
  
  Usage example:
    $ renm "abc[a|d+]def.txt" "bla[a]blubb.txt"
    
    Renames all files starting with "abc", ending with "def.txt" with
    one of more digits inbetween (match called a) into files "bla" + a + "blub.txt".
    
  Note:
    Finds and searches only through the files in the current directory
  
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
        -- TODO: check for correctness of arguments. parse (parseStrWith bodyInnerExp) "pattern" s1
        xs <- listDirectory "."
        runEffect $ each xs >-> pipeline s1 s2
  {-withParseResult argParser-}
  prog (A "bla.txt" "blubb.txt")
;

-- In ghci> prog (A "bla.txt" "blubb.txt")


{- === Parse pattern and out-filenames === -}
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

parseStrWith :: Parser a -> Parser [Either String a]
parseStrWith p = chainl
  ((:[]) . Left <$> txt)
  (do cont <- p;
      return (\xs1 xs2 -> xs1 ++ [Right cont] ++ xs2))
  []

testPrs prsexp prsstr =
  (\p -> parse p "inner" prsstr) <$> parse prs "outer" prsexp

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
exp = many many
many = base "+" | base "*"
base = "d" | "c" | "a" | "(" exp ")"

TODO: add useful capabilities like a real reg-exp parser does
-}

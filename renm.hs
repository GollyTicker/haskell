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

    Escape [ in filesnames via \. e.g. [Hi].txt becomes "\\[Hi].txt"
  
  Note:
    Searches and replaces only through the files in the current directory.
  
  TODO:
    . Illustrate with many concrete examples
    . Describe inner expression matcher language.
    . find concrete use cases and improve towards usability
    . introduce backtracked parsing. cases like "[a|f+]c[b|f+]b.txt"
        on "aaccddb.txt" should return a=aac and b=dd. (Greediest match that works)
    DONE add support for substrings. e.g. "[a|f+'hello'f+]" should match
        "bl-ahellob-lubb". input an escaped ' via \\'
    . AUTO-COMPLETION / TAB-SUGGESTIONS
    . History von kürzlichen verwendeten Matches/Creators
    . Add proper test suite
    . add useful capabilities like a real reg-exp parser does.
        e.g. ranges like 0-9 and co.
    . distribute binaries on various distris. cross-compile via ghc options
      -> make apt-get install-able
    . check for duplicates in file renamings -> show to ppl
    DONE escaping of [ with "\\["
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

import Debug.Trace


{- === MAIN, Argument Parsing, User Interface === -}

main :: IO ()
main = withParseResult argParser prog

data A = A String String Bool deriving (Show)
argParser :: ParserSpec A
argParser = A `parsedBy` reqPos "matcher" `Descr` "file-name pattern to match against"
              `andBy` reqPos "creator" `Descr` "pattern according to which the new file-names are created"
              `andBy` boolFlag "dry" `Descr` "dry-run. Shows renaming without actually changing any files."

prog :: A -> IO ()
prog (A s1 s2 dry) = do
  when dry $
    putStrLn "[ This is a dry run showing what would be done. Nothing will be replaced. ]"
  case parseMatCrt s1 s2 of
    Right (mat,crt) -> do
      putStrLn $ concat ["Understood matcher ",s1," and creator ",s2,"."]
      xs <- listDirectory "."
      runEffect $ each xs >-> tryReplace mat crt dry
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
   prog (A "dryrun-bla.txt" "blubb.txt" True)
  ,prog (A "[a|a+].[b|a+]" "[a].[b]" True)
  ,prog (A "[a|a+]-[b|a+]" "[a]-[b]" True)
  ,prog (A "dryrun[a|bad-expression].txt" "blubb[a].txt" True)
  ,prog (A "dryrun[a|a][a|a]" "a" True)
  ,prog (A "dryrun[a|a]" "[b]" True)
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

tryReplace :: Matcher -> Creator -> Bool -> Consumer String IO ()
tryReplace mat crt dry = forever $ do
  nm <- await
  every (fileExists nm)
    >-> matchTransform mat crt
    >-> renmFile dry

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

renmFile :: Bool -> Consumer (String,String) IO ()
renmFile dry = do
  (x,y) <- await
  lift $ putStrLn $ concat ["Rename ",if dry then " [dry] " else " ", x, " -> ", y]
  lift $ when (not dry) $ renameFile x y


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
txt = many (escape <|> noneOf "[")

escape :: Parser Char
escape = char '\\' >> anyChar

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


inExprStr :: PrsParser
inExprStr = many (escape <|> noneOf "'")
            >>= return . string

{- === Inner Expression Parser === -}
type Prs = Parser String
type PrsParser = Parser Prs
prs :: PrsParser
prs =
  let withLA :: PrsParser ->  PrsParser
      withLA prec = atomic "d" digit
                <|> atomic "c" letter
                <|> atomic "a" alphaNum
                <|> atomic "s" space
                <|> atomic "." anyChar
                <|> atomic "f" (oneOf " -()" <|> alphaNum)
                <|> between (string "'") (string "'") inExprStr
                <|> between (string "(") (string ")") prec
      
      withMany :: PrsParser -> PrsParser
      withMany pp = 
        do p <- pp
           fs <- many (choice [
                   string "+" *> return (fmap concat . many1)
                  ,string "*" *> return (fmap concat . many )
                  --,string "?" *> return (fmap concat . many )
                  ,let calcInt =
                        snd
                        . foldr (\x (b,n) -> (b*10,b*x+n)) (1,0)
                        . map (read . (:[]))
                  in  do n <- calcInt <$> many1 digit
                         return (fmap concat . count n)
                  ])
           
           return $ foldr (flip (.)) id fs p
      seq = do ps <- many $ withMany (withLA prs)
               return $ concat <$> sequence ps
      --vbar = string "|" >> return (\pl pr -> try pl <|> pr)
  in  seq -- chainl seq vbar (string "")

atomic :: String -> Parser Char -> PrsParser
atomic s p = string s >> return ((:[]) <$> p)


-- full regular expressions would need Potenzmengenkonstruktion.
-- ( )+ one of more
-- ( )* zero or more
-- ( )? zero or one (NOT IMPL.)
-- ( )n repeats n times where n is a number
-- (|) left. if not, then right.
-- d for digits
-- c for characters
-- a for alphanumeric
-- s for space
-- . for wildcard
-- f for filename. alphanumeric with space, dash (-),
--       lpar and rpar (,)
{-
exp = many* -- means zero or more times
many = base "+" | base "*" | base number
base = "d" | "c" | "a" | "(" exp ")"
-}

innerExprProg :: IO ()
innerExprProg = do
  let f e s = do
        putStrLn $ concat ["Running expr ",e," on ",s]
        print $ testPrs e s
        putStrLn " == "
      exps_strs = zip
        ["(c+d+)+","(cd*c)+"
          ,"d3+","d3+","d3+","d3+"]
        ["ab12cd34ef56blabla423423","a1ab2323be12312e",
          "123","1234","12345","123456"]
  mapM_ (uncurry f) exps_strs

testPrs prsexp prsstr =
  (\p -> parse (p<*eof) "inner" prsstr)
  <$> parse (prs<*eof) "outer" prsexp


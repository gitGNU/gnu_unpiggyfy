module Main where

import Data.List (intersperse)
import System.Environment (getArgs)

import Languages

import CommentRemoval (hltToString)
import CommentRemoval (isOnlyIndentationLine)
import CommentRemoval (rmCmtsWrapper)
import CommentRemoval (lowLevelTokenizeWrapper, highLevelTokenizeWrapper)
import CommentRemoval (tokenizeCodeWrapper)
import CommentRemoval (compressCodeWrapper)
import CommentRemoval (weakCompress)

usage :: IO ()
usage = (error . concat . (intersperse "\n"))
            ["usage"
            ," -h | --help    -> what you are reading now"
            ," -rc  file-name -> remove comments"
            ," -llt file-name -> tokenize low level"
            ," -hlt file-name -> tokenize high level"
            ," -tc  file-name -> tokenize code only"
            ," -cc  file-name -> compress code"]

main :: IO ()
main =
    do args <- getArgs
--       mapM_ print args -- uncomment to inspect command line
       case args of
         [] -> usage
         x:xs ->
             if x == "-rc"
             then do res <- rmCmtsWrapper (enforceFileParam xs)
                                          haskellParseInfo
                     let res' = filter (not . onlyIndentOrNull) res
                         res'' = map (map hltToString) res'
                     mapM_ putStrLn (map concat res'')
             else if x == "-llt"
             then do res <- lowLevelTokenizeWrapper (enforceFileParam xs)
                                                    haskellParseInfo
                     mapM_ putStrLn (map show res)
             else if x == "-hlt"
             then do res <- highLevelTokenizeWrapper (enforceFileParam xs)
                                                     haskellParseInfo
                     mapM_ putStrLn (map show res)
             else if x == "-tc"
             then do res <- tokenizeCodeWrapper (enforceFileParam xs)
                                                haskellParseInfo
                     mapM_ putStrLn (map show res)
             else if x == "-cc"
             then do res <- compressCodeWrapper (enforceFileParam xs)
                                                weakCompress
                                                haskellParseInfo
                     mapM_ putStrLn res
             else usage
    where
      onlyIndentOrNull x = null x || isOnlyIndentationLine x

      enforceFileParam [] = error "no file name given"
      enforceFileParam (f:_) = f

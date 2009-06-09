module Main where

import Data.List (intersperse)
import System.Environment (getArgs)

import CommentRemoval (hltToString)
import CommentRemoval (rmCmtsWrapper)
import CommentRemoval (lowLevelTokenizeWrapper, highLevelTokenizeWrapper)

usage :: IO ()
usage = (error . concat . (intersperse "\n"))
            ["usage"
            ," -h | --help"
            ," -rc  file-name  -> remove comments"
            ," -ltok file-name -> low level tokenization"
            ," -htok file-name -> high level tokenization"]

main :: IO ()
main =
    do args <- getArgs
--       mapM_ print args -- uncomment to inspect command line
       case args of
         [] -> usage
         x:xs ->
             if x == "--help" || x == "-h"
             then usage
             else if x == "-rc"
             then do res <- rmCmtsWrapper (head xs)
                     let res' = map (map hltToString) res
                     mapM_ putStrLn (map concat res')
                     -- FBR: @TODO @BUG remove empty lines of former comments!
                     --      !!! ShortCmt are factorized too much in hl tokens:
                     --          the next hl token  of code is seen as a short
                     --          comment
             else if x == "-ltok"
             then do res <- lowLevelTokenizeWrapper (head xs)
                     mapM_ putStrLn (map show res)
             else if x == "-htok"
             then do res <- highLevelTokenizeWrapper (head xs)
                     mapM_ putStrLn (map show res)
             else usage

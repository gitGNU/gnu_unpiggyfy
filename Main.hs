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
             if x == "-rc"
             then do res <- rmCmtsWrapper (head xs)
                     let res' = map (map hltToString) res
                     mapM_ putStrLn (map concat res')
             else if x == "-ltok"
             then do res <- lowLevelTokenizeWrapper (head xs)
                     mapM_ putStrLn (map show res)
             else if x == "-htok"
             then do res <- highLevelTokenizeWrapper (head xs)
                     mapM_ putStrLn (map show res)
             else usage

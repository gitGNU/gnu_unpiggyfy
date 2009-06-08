module Main where

import System.Environment (getArgs)

import CommentRemoval (hltToString)
import CommentRemoval (rmCmtsWrapper)
import CommentRemoval (lowLevelTokenizeWrapper, highLevelTokenizeWrapper)

usage :: IO ()
usage = error ("usage\n" ++ -- FBR: use intercolate for \n insertion
               " -h | --help\n" ++
               " -rc  file-name -> remove comments\n" ++
               " -ltok file-name -> low level tokenization\n" ++
               " -htok file-name -> high level tokenization\n")

main :: IO ()
main =
    do args <- getArgs
--       mapM_ print args
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
             else if x == "-ltok"
             then do res <- lowLevelTokenizeWrapper (head xs)
                     mapM_ putStrLn (map show res)
             else if x == "-htok"
             then do res <- highLevelTokenizeWrapper (head xs)
                     mapM_ putStrLn (map show res)
             else usage

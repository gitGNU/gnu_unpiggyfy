module Main where

import System.Environment (getArgs)

import CommentRemoval (hltToString, rmCmtsWrapper)

usage :: IO ()
usage = error ("usage: unpig [-rc file-name] # remove comments")

main :: IO ()
main =
    do args <- getArgs
--       mapM_ print args
       case args of
         [] -> usage
         x:xs ->
             if x == "-rc"
             then do res <- rmCmtsWrapper (head xs)
                     let res' = map (map hltToString) res
                     mapM_ putStrLn (map concat res')
             else usage

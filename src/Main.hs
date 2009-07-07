module Main where

import Data.List (intersperse)
import Data.Maybe (Maybe)
import System.Environment (getArgs)

import Languages

import CommentRemoval (hltToString)
import CommentRemoval (isOnlyIndentationLine)
import CommentRemoval (rmCmtsWrapper)
import CommentRemoval (lowLevelTokenizeWrapper, highLevelTokenizeWrapper)
import CommentRemoval (tokenizeCodeWrapper)
import CommentRemoval (compressCodeWrapper)
import CommentRemoval (weakCompress)

data Mode = Help
          | RemoveComments
          | TokenizeLowLevel
          | TokenizeHighLevel
          | TokenizeCode
          | CompressCode
          deriving Show

type Options = (Mode, Maybe SupportedLanguage, Maybe FilePath)

usage :: IO ()
usage = (error . concat . (intersperse "\n"))
            ["usage:"
            ," { -h } | { -lg { C | HS } -{ rc | llt | hlt | tc | cc }"
            ,"            -i FILE }"
            ," h | help  -> what you are reading now"
            ," lg {C,HS}      -> your source code's language"
            ,"   rc           -> remove comments"
            ,"   llt          -> low level tokenization"
            ,"   hlt          -> high level tokenization"
            ,"   tc           -> tokenize code"
            ,"   cc           -> compress code"
            ,"   i input-file -> source file to read from"
            ,"   ### HS stands for Haskell"]

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

enforceStringParam [] = error "no string given"
enforceStringParam (f:_) = f

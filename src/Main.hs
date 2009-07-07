module Main where

import Data.List (intersperse)
import Data.Maybe (Maybe)

import System.Console.GetOpt
import System
import Control.Monad
import IO
import List
import Char

import Languages

import CommentRemoval (hltToString)
import CommentRemoval (isOnlyIndentationLine)
import CommentRemoval (rmCmtsWrapper)
import CommentRemoval (lowLevelTokenizeWrapper, highLevelTokenizeWrapper)
import CommentRemoval (tokenizeCodeWrapper)
import CommentRemoval (compressCodeWrapper)
import CommentRemoval (weakCompress)

data Mode = RemoveComments
          | TokenizeLowLevel
          | TokenizeHighLevel
          | TokenizeCode
          | CompressCode
          deriving Show

type LangSourceFile = (SupportedLanguage, FilePath)

data ParsedCommandLine = Help
                       | Action Mode LangSourceFile
                       deriving Show

-- args become either an error message or a parsed command line
parseCommandLine :: [String]
                 -> Maybe SupportedLanguage -> Maybe FilePath -> Maybe Mode
                 -> Either String ParsedCommandLine
parseCommandLine [] Nothing Nothing _       =
    Left "no language and filename provided"
parseCommandLine [] Nothing _       _       =
    Left "unsupported or unprovided language option"
parseCommandLine [] _       Nothing _       =
    Left "no filename provided"
parseCommandLine [] _       _       Nothing =
    Left "no mode provided"
parseCommandLine [] (Just lg) (Just filename) (Just mode) =
    Right (Action mode (lg, filename))
parseCommandLine (arg:args) lg filename mode =
    case arg of
      "-rc"  ->   parseCommandLine args lg filename (Just RemoveComments)
      "-llt" ->   parseCommandLine args lg filename (Just TokenizeLowLevel)
      "-hlt" ->   parseCommandLine args lg filename (Just TokenizeHighLevel)
      "-tc"  ->   parseCommandLine args lg filename (Just TokenizeCode)
      "-cc"  ->   parseCommandLine args lg filename (Just CompressCode)
      "-h"     -> Right Help
      "--help" -> Right Help
      _        -> if isPrefixOf langPrfx arg then
                      parseCommandLine
                        args
                        (findLanguage (drop (length langPrfx) arg))
                        filename mode
                  else if isPrefixOf inputPrfx arg then
                      parseCommandLine
                        args
                        lg
                        (maybeFilename (drop (length inputPrfx) arg))
                        mode
                  else Right Help
    where
      langPrfx  = "-lg:"
      inputPrfx = "-i:"

usage :: IO ()
usage = (error . concat . (intersperse "\n"))
            ["usage:"
            ," { -h } | { -lg: { C | HS } -{ rc | llt | hlt | tc | cc }"
            ,"            -i:FILE }"
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
--     mapM_ print args -- uncomment to inspect command line
       case parseCommandLine args Nothing Nothing Nothing of
         Left errMsg -> do hPutStrLn stderr ("unpig: error:" ++ errMsg)
                           usage
         Right Help -> usage
         Right (Action RemoveComments    (lang, file)) ->
             do res <- rmCmtsWrapper file (getParseInfo lang)
                let res'  = filter (not . onlyIndentOrNull) res
                    res'' = map (map hltToString) res'
                mapM_ putStrLn (map concat res'')
         Right (Action TokenizeLowLevel  (lang, file)) ->
             do res <- lowLevelTokenizeWrapper file (getParseInfo lang)
                mapM_ putStrLn (map show res)
         Right (Action TokenizeHighLevel (lang, file)) ->
             do res <- highLevelTokenizeWrapper file (getParseInfo lang)
                mapM_ putStrLn (map show res)
         Right (Action TokenizeCode      (lang, file)) ->
             do res <- tokenizeCodeWrapper file (getParseInfo lang)
                mapM_ putStrLn (map show res)
         Right (Action CompressCode      (lang, file)) ->
             do res <- compressCodeWrapper file weakCompress
                                           (getParseInfo lang)
                mapM_ putStrLn res
    where
      onlyIndentOrNull x = null x || isOnlyIndentationLine x

enforceFileParam [] = error "no file name given"
enforceFileParam (f:_) = f

enforceStringParam [] = error "no string given"
enforceStringParam (f:_) = f

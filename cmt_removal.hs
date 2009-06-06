import Control.Exception
import Data.Maybe
import Data.List
import System.IO

-- This program is free
-- software; you may redistribute and/or modify it under the terms of the
-- GNU General Public License as published by the Free Software
-- Foundation; Version 2 with the clarifications and exceptions described
-- below.  This guarantees your right to use, modify, and redistribute
-- this software under certain conditions.  If you wish to embed this
-- technology into proprietary software, we sell alternative licenses
-- (contact Francois BERENGER, the Unpiggyfy's project admin and owner).
-- By contributing changes to Unpiggyfy, you are offering the Unpiggyfy
-- Project (owned by Francois BERENGER) the unlimited,
-- non-exclusive right to reuse, modify, and relicense the code.  Unpiggyfy
-- will always be available Open Source, but this is important because the
-- inability to relicense code has caused devastating problems for other
-- Free Software projects (such as KDE and NASM).  We also occasionally
-- relicense the code to third parties as discussed above.
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License v2.0 for more details at
-- http://www.gnu.org/licenses/gpl-2.0.html

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

type Token       = String
type CodeStr     = String
type ShortCmtStr = String
type LongCmtStr  = String

-- A source file can be seen as a list of text lines. Each one
-- being made of different items :
-- source code, comments, comment delimiters, etc.
data SrcLine = Code         CodeStr -- <high level>
             | ShortCmt ShortCmtStr
             | LongCmt  LongCmtStr  -- </high level>
             | CmtOrCode    String  -- <low level>
             | BeginCmtMark Token
             | EndCmtMark   Token
             | LineCmtMark  Token   -- </low level>
             deriving Show

data ParserState = ReadingCode
                 | ReadingShortCmt
                 | ReadingLongCmt
-- @TODO FBR: to not trap the parser with "fake" tokens embedded in
--            String consts, handle these soon:
--               | ReadingStringInCode
--               | ReadingStringInShortCmt
--               | ReadingStringInLongCmt

srcLineToString :: SrcLine -> String
srcLineToString (Code         s) = s
srcLineToString (ShortCmt     s) = s
srcLineToString (LongCmt      s) = s
srcLineToString (CmtOrCode    s) = s
srcLineToString (BeginCmtMark s) = s
srcLineToString (EndCmtMark   s) = s
srcLineToString (LineCmtMark  s) = s

startWithList :: [Token] -> String -> Maybe Token
-- FBR: use a map here instead of explicit recursion ?
startWithList [] str            = Nothing
startWithList (prfx:others) str
    | isPrefixOf prfx str = Just prfx
    | otherwise           = startWithList others str

-- the remaining string is modified by a token or by a letter consumption
firstMatchedTok :: String -> [Token] -> [Token] -> [Token]
                -> Maybe (SrcLine, String)
firstMatchedTok [] _ _ _ = Nothing
firstMatchedTok str@(c:cs) cmtStartList cmtEndList lineCmtList =
    case startWithList cmtStartList str of
      Just matched ->
          Just (BeginCmtMark matched
               ,consumeTokenUnsafe matched str)
      Nothing ->
          case startWithList cmtEndList str of
            Just matched ->
                Just (EndCmtMark matched
                     ,consumeTokenUnsafe matched str)
            Nothing ->
                case startWithList lineCmtList str of
                  Just matched ->
                      Just (LineCmtMark matched
                           ,consumeTokenUnsafe matched str)
                  Nothing -> Just (CmtOrCode (c:[]), cs)
    where
      -- !!! tok must be a prefix of str !!!
      consumeTokenUnsafe :: Token -> String -> String
      consumeTokenUnsafe tok str = drop (length tok) str

-- line to list of tokens
tokenizeSrcLine :: [Token] -> [Token] -> [Token] -> [SrcLine]
                -> String -> [SrcLine]
tokenizeSrcLine cmtStartList cmtEndList lineCmtList acc srcLine =
    case firstMatchedTok srcLine cmtStartList cmtEndList lineCmtList of
      Nothing -> reverse acc
      Just (tok, remaining) -> tokenizeSrcLine
                                 cmtStartList cmtEndList lineCmtList
                                 (tok:acc) remaining

-- file name to list of lines read
getLines :: String -> IO [String]
getLines fileName =
    do mfh <- try (openFile fileName ReadMode)
       case mfh of
         Left err -> do print ("cannot open file named " ++ fileName ++
                               ": " ++ (show err))
                        return []
         Right handle ->
             do getLines' handle []
    where
      getLines' :: Handle -> [String] -> IO [String]
      getLines' handle acc =
          do mline <- try (hGetLine handle)
             case mline of
               Left err -> return (reverse acc)
               Right line -> getLines' handle (line:acc)

-- file name to list of low level tokens
tokenizeFile :: String -> [Token] -> [Token] -> [Token]
             -> IO [[SrcLine]]
tokenizeFile fileName cmtStartList cmtEndList lineCmtList =
    do lines <- getLines fileName
       return (map (tokenizeSrcLine cmtStartList cmtEndList lineCmtList [])
                   lines)

-- low to high level tokens
highLevelTokens :: [[SrcLine]] -> ParserState -> Int -> [SrcLine] -> [[SrcLine]]
                -> [[SrcLine]]
highLevelTokens [] _ _ _ acc = reverse acc
highLevelTokens (line:others) parserState depth lineAcc acc =
    case line of
      -- finished processing current line
      [] -> case parserState of
              -- reading a short comment is a state valid only on one line
              -- until we know more about the next line
              ReadingShortCmt -> highLevelTokens others ReadingCode depth []
                                                 ((reverse lineAcc):acc)
              _ -> highLevelTokens others parserState depth []
                                   ((reverse lineAcc):acc)
      -- continuing processing current line
      (tok:toks) ->
          case parserState of
            ReadingCode ->
                case tok of
                  (CmtOrCode code) ->
                      highLevelTokens
                        (toks:others) parserState depth
                        ((Code code):lineAcc) acc
                  (LineCmtMark mark) ->
                      highLevelTokens
                        (toks:others) ReadingShortCmt depth
                        ((ShortCmt mark):lineAcc) acc
                  (BeginCmtMark mark) ->
                      highLevelTokens
                        (toks:others) ReadingLongCmt (depth+1)
                        ((LongCmt mark):lineAcc) acc
            ReadingShortCmt ->
                highLevelTokens (toks:others) ReadingShortCmt depth
                                ((ShortCmt (srcLineToString tok)):lineAcc)
                                acc
            ReadingLongCmt ->
                case tok of
                  (EndCmtMark mark) ->
                      let newDepth = depth - 1 in
                      if newDepth == 0 then
                          highLevelTokens (toks:others) ReadingCode newDepth
                                          ((LongCmt mark):lineAcc) acc
                      else
                          highLevelTokens
                            (toks:others) parserState newDepth
                            ((LongCmt mark):lineAcc) acc
                  (BeginCmtMark mark) ->
                      let newDepth = depth + 1 in
                      highLevelTokens (toks:others) parserState newDepth
                                      ((LongCmt mark):lineAcc) acc

-- several consecutive high level tokens from the same line are combined into
-- only one
factorize :: [[SrcLine]] -> ParserState
          -> [CodeStr] -> [ShortCmtStr] -> [LongCmtStr]
          -> [SrcLine]
          -> [SrcLine]
factorize [] _ _ _ _ acc = reverse acc
factorize (line:others) parserState
          codeAcc shortCmtAcc longCmtAcc acc = -- undefined
    case line of
      -- finished processing current line
      [] ->
          case parserState of
            ReadingCode ->
                factorize others parserState
                          [] shortCmtAcc longCmtAcc
                          ((Code (concat (reverse codeAcc))):acc)
            ReadingShortCmt ->
                -- reading a short comment is a state valid only on one line
                -- until we know more about the next line
                factorize others ReadingCode
                          codeAcc [] longCmtAcc
                          ((ShortCmt (concat (reverse shortCmtAcc))):acc)
            ReadingLongCmt ->
                factorize others parserState
                          codeAcc shortCmtAcc []
                          ((LongCmt (concat (reverse longCmtAcc))):acc)
      -- continuing processing current line
      (tok:toks) ->
          case parserState of
            ReadingCode ->
                case tok of
                  (Code code) ->
                      factorize (toks:others) parserState
                                (code:codeAcc) shortCmtAcc longCmtAcc acc
                  (ShortCmt cmt) ->
                      factorize (toks:others) ReadingShortCmt
                                codeAcc (cmt:shortCmtAcc) longCmtAcc acc
                  (LongCmt cmt) ->
                      factorize (toks:others) ReadingLongCmt
                                codeAcc shortCmtAcc (cmt:longCmtAcc) acc
            ReadingShortCmt ->
                case tok of
                  (Code code) ->
                      factorize (toks:others) ReadingCode
                                (code:codeAcc) shortCmtAcc longCmtAcc acc
                  (ShortCmt cmt) ->
                      factorize (toks:others) parserState
                                codeAcc (cmt:shortCmtAcc) longCmtAcc acc
                  (LongCmt cmt) ->
                      factorize (toks:others) ReadingLongCmt
                                codeAcc shortCmtAcc (cmt:longCmtAcc) acc
            ReadingLongCmt ->
                case tok of
                  (Code code) ->
                      factorize (toks:others) ReadingCode
                                (code:codeAcc) shortCmtAcc longCmtAcc acc
                  (ShortCmt cmt) ->
                      factorize (toks:others) ReadingShortCmt
                                codeAcc (cmt:shortCmtAcc) longCmtAcc acc
                  (LongCmt cmt) ->
                      factorize (toks:others) parserState
                                codeAcc shortCmtAcc (cmt:longCmtAcc) acc

isLongCmt (LongCmt _) = True
isLongCmt _           = False

isShortCmt (ShortCmt _) = True
isShortCmt _            = False

isCode (Code _) = True
isCode _        = False

isCmt x = isShortCmt x || isLongCmt x

removeComments :: [SrcLine] -> [SrcLine]
removeComments srcLine = filter isCode srcLine

-- test tokenization
test :: IO [SrcLine]
test =
    do lowLevelToks <- tokenizeFile "cmt_removal.hs" [] [] ["--"]
       let highLevelToks = highLevelTokens lowLevelToks ReadingCode 0 [] []
           factorized = factorize highLevelToks ReadingCode [] [] [] []
           codeOnly = removeComments factorized
       return codeOnly

-- TODO: FBR: write a unix tool removing comments language independently

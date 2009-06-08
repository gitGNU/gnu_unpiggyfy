import Control.Exception
import Data.Maybe
import Data.List
import System.IO

-- This program is free software; you may redistribute and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation; Version 2 with the clarifications and exceptions
-- described below.
-- This guarantees your right to use, modify, and redistribute
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

type Token       = String
type CodeStr     = String
type ShortCmtStr = String
type LongCmtStr  = String

-- A source file can be seen as a list of text lines. Each one
-- being made of different items :
-- source code, comments, comment delimiters, etc.
data LowLevelToken = CmtOrCode    Char
                   | BeginCmtMark Token
                   | EndCmtMark   Token
                   | LineCmtMark  Token
                   deriving Show

data HighLevelToken = Code     CodeStr
                    | ShortCmt ShortCmtStr
                    | LongCmt  LongCmtStr
                    deriving Show

data ParserState = ReadingCode
                 | ReadingShortCmt
                 | ReadingLongCmt
-- @TODO FBR: to not trap the parser with "fake" tokens embedded in
--            String consts, handle these soon:
--               | ReadingStringInCode
--               | ReadingStringInShortCmt
--               | ReadingStringInLongCmt
-- @TODO: FBR: write a unix CLI tool removing comments language independently

lltToString :: LowLevelToken -> String
lltToString (CmtOrCode    s) = s:[]
lltToString (BeginCmtMark s) = s
lltToString (EndCmtMark   s) = s
lltToString (LineCmtMark  s) = s

promote :: ParserState -> LowLevelToken -> HighLevelToken
promote ReadingCode     (CmtOrCode    x) = Code (x:[])
promote ReadingShortCmt (CmtOrCode    x) = ShortCmt (x:[])
promote ReadingLongCmt  (CmtOrCode    x) = LongCmt (x:[])
promote _ (BeginCmtMark x) = LongCmt  x
promote _ (EndCmtMark   x) = LongCmt  x
promote _ (LineCmtMark  x) = ShortCmt x

startWithList :: [Token] -> String -> Maybe Token
-- FBR: use a map here instead of explicit recursion ?
startWithList [] _ = Nothing
startWithList (prfx:others) str
    | isPrefixOf prfx str = Just prfx
    | otherwise           = startWithList others str

-- the remaining string is modified by a token or by a letter consumption
firstMatchedTok :: String -> [Token] -> [Token] -> [Token]
                -> Maybe (LowLevelToken, String)
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
                  Nothing -> Just (CmtOrCode c, cs)
    where
      -- !!! tok _MUST_ be a prefix of str !!!
      consumeTokenUnsafe :: Token -> String -> String
      consumeTokenUnsafe tok tokPrefixedStr = drop (length tok) tokPrefixedStr

-- line to list of tokens
tokenizeSrcLine :: [Token] -> [Token] -> [Token]
                -> [LowLevelToken] -> String -> [LowLevelToken]
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
         Right h ->
             do getLines' h []
    where
      getLines' :: Handle -> [String] -> IO [String]
      getLines' h acc =
          do mline <- try (hGetLine h)
             case mline of
               Left _ -> return (reverse acc)
               Right line -> getLines' h (line:acc)

-- file name to list of low level tokens
tokenizeFile :: String -> [Token] -> [Token] -> [Token]
             -> IO [[LowLevelToken]]
tokenizeFile fileName cmtStartList cmtEndList lineCmtList =
    do readLines <- getLines fileName
       return (map (tokenizeSrcLine cmtStartList cmtEndList lineCmtList [])
                   readLines)

-- low to high level tokens
highLevelTokens :: [[LowLevelToken]] -> ParserState -> Int
                -> [HighLevelToken] -> [[HighLevelToken]]
                -> [[HighLevelToken]]
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
                  CmtOrCode _ ->
                      highLevelTokens (toks:others) parserState depth
                                      (factorize parserState tok lineAcc) acc
                  LineCmtMark _ ->
                      highLevelTokens (toks:others) ReadingShortCmt depth
                                      (factorize parserState tok lineAcc) acc
                  BeginCmtMark _ ->
                      highLevelTokens (toks:others) ReadingLongCmt (depth+1)
                                      (factorize parserState tok lineAcc) acc
                  EndCmtMark mark ->
                      error ("got EndCmtMArk while ReadingCode: " ++ mark)
            ReadingShortCmt ->
                highLevelTokens (toks:others) ReadingShortCmt depth
                                (factorize parserState tok lineAcc) acc
            ReadingLongCmt ->
                case tok of
                  EndCmtMark _ ->
                      let newDepth = depth - 1 in
                      if newDepth == 0 then
                          highLevelTokens (toks:others) ReadingCode newDepth
                                          (factorize parserState tok lineAcc)
                                          acc
                      else
                          highLevelTokens
                            (toks:others) parserState newDepth
                            (factorize parserState tok lineAcc) acc
                  BeginCmtMark _ ->
                      let newDepth = depth + 1 in
                      highLevelTokens (toks:others) parserState newDepth
                                      (factorize parserState tok lineAcc) acc
                  CmtOrCode _ ->
                      highLevelTokens (toks:others) parserState depth
                                      (factorize parserState tok lineAcc) acc
                  LineCmtMark mark ->
                      error ("got LineCmtMark while ReadingLongCmt: " ++ mark)

factorize :: ParserState -> LowLevelToken -> [HighLevelToken]
          -> [HighLevelToken]
factorize state elt [] = (promote state elt):[]
factorize state elt lst@(x:xs) =
    case elt of
      CmtOrCode _ ->
          case x of
            Code code -> (Code (code ++ (lltToString elt))):xs
            _         -> (promote state elt):lst
      BeginCmtMark _ ->
          case x of
            LongCmt longCmt -> (LongCmt (longCmt ++ (lltToString elt))):xs
            _               -> (promote state elt):lst
      EndCmtMark _ ->
          case x of
            LongCmt longCmt -> (LongCmt (longCmt ++ (lltToString elt))):xs
            _               -> (promote state elt):lst
      LineCmtMark _ ->
          case x of
            ShortCmt shortCmt -> (ShortCmt (shortCmt ++ (lltToString elt))):xs
            _                 -> (promote state elt):lst

isCode :: HighLevelToken -> Bool
isCode (Code _) = True
isCode _        = False

removeComments :: [[HighLevelToken]] -> [[HighLevelToken]]
removeComments srcLine = map (filter isCode) srcLine

-- test tokenization
rmCmtsWrapper :: IO [[HighLevelToken]]
rmCmtsWrapper =
    do lowLevelToks <- tokenizeFile "cmt_removal.hs" [] [] ["--"]
       let highLevelToks = highLevelTokens lowLevelToks ReadingCode 0 [] []
           codeOnly = removeComments highLevelToks
       return codeOnly

main :: IO ()
main = do res <- rmCmtsWrapper
          print res

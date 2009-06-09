module CommentRemoval (
    HighLevelToken,
    hltToString,
    rmCmtsWrapper,
    lowLevelTokenizeWrapper,
    highLevelTokenizeWrapper
) where

import Control.Exception
import Data.Maybe
import Data.List
import System.IO

type Token       = String
type CodeStr     = String
type ShortCmtStr = String
type LongCmtStr  = String

-- A source file can be seen as a list of text lines. Each one
-- being made of different items :
-- source code, comments, comment delimiters, etc.
data LowLevelToken = CmtOrCodeOrString Char
                   | CmtBegin          Token
                   | CmtEnd            Token
                   | LineCmtMark       Token
                   | StringBeginOrEnd  Token
                   deriving Show

data HighLevelToken = Code     CodeStr
                    | ShortCmt ShortCmtStr
                    | LongCmt  LongCmtStr
                    | StringInCode     CodeStr
                    | StringInShortCmt ShortCmtStr
                    | StringInLongCmt  LongCmtStr
                    deriving Show

data ParserState = ReadingCode
                 | ReadingShortCmt
                 | ReadingLongCmt
                 | ReadingStringInCode
                 | ReadingStringInShortCmt
                 | ReadingStringInLongCmt

lltToString :: LowLevelToken -> String
lltToString (CmtOrCodeOrString s) = s:[]
lltToString (CmtBegin          s) = s
lltToString (CmtEnd            s) = s
lltToString (LineCmtMark       s) = s
lltToString (StringBeginOrEnd  s) = s

hltToString :: HighLevelToken -> String
hltToString (Code         s) = s
hltToString (ShortCmt     s) = s
hltToString (LongCmt      s) = s
hltToString (StringInCode     s) = s
hltToString (StringInShortCmt s) = s
hltToString (StringInLongCmt  s) = s

promote :: ParserState -> LowLevelToken -> HighLevelToken
promote ReadingCode             (CmtOrCodeOrString x) = Code             (x:[])
promote ReadingShortCmt         (CmtOrCodeOrString x) = ShortCmt         (x:[])
promote ReadingLongCmt          (CmtOrCodeOrString x) = LongCmt          (x:[])
promote ReadingStringInCode     (CmtOrCodeOrString x) = StringInCode     (x:[])
promote ReadingStringInShortCmt (CmtOrCodeOrString x) = StringInShortCmt (x:[])
promote ReadingStringInLongCmt  (CmtOrCodeOrString x) = StringInLongCmt  (x:[])
promote ReadingCode             (StringBeginOrEnd x)  = StringInCode     x
promote ReadingShortCmt         (StringBeginOrEnd x)  = StringInShortCmt x
promote ReadingLongCmt          (StringBeginOrEnd x)  = StringInLongCmt  x
promote ReadingStringInCode     (StringBeginOrEnd x)  = StringInCode     x
promote ReadingStringInShortCmt (StringBeginOrEnd x)  = StringInShortCmt x
promote ReadingStringInLongCmt  (StringBeginOrEnd x)  = StringInLongCmt  x
promote _                       (CmtBegin    x)       = LongCmt          x
promote _                       (CmtEnd      x)       = LongCmt          x
promote ReadingStringInCode     (LineCmtMark x)       = StringInCode     x
promote _                       (LineCmtMark x)       = ShortCmt         x

startWithList :: [Token] -> String -> Maybe Token
-- FBR: use a map here instead of explicit recursion ?
startWithList [] _ = Nothing
startWithList (prfx:others) str
    | isPrefixOf prfx str = Just prfx
    | otherwise           = startWithList others str

-- the remaining string is modified by a token or by a letter consumption
firstMatchedTok :: String
                -> [Token] -> [Token] -> [Token] -> [Token]
                -> Maybe (LowLevelToken, String)
firstMatchedTok [] _ _ _ _ = Nothing
firstMatchedTok str@(c:cs) cmtStarters cmtStopers shortCmtStarters
                stringDelims =
    case startWithList stringDelims str of
      Just matched ->
          Just (StringBeginOrEnd matched
               ,consumeTokenUnsafe matched str)
      Nothing ->
          case startWithList cmtStarters str of
            Just matched ->
                Just (CmtBegin matched
                     ,consumeTokenUnsafe matched str)
            Nothing ->
                case startWithList cmtStopers str of
                  Just matched ->
                      Just (CmtEnd matched
                           ,consumeTokenUnsafe matched str)
                  Nothing ->
                      case startWithList shortCmtStarters str of
                        Just matched ->
                            Just (LineCmtMark matched
                                 ,consumeTokenUnsafe matched str)
                        Nothing -> Just (CmtOrCodeOrString c, cs)
    where
      -- !!! tok _MUST_ be a prefix of str !!!
      consumeTokenUnsafe :: Token -> String -> String
      consumeTokenUnsafe tok tokPrefixedStr = drop (length tok) tokPrefixedStr

-- line to list of tokens
tokenizeSrcLine :: [Token] -> [Token] -> [Token] -> [Token]
                -> [LowLevelToken] -> String -> [LowLevelToken]
tokenizeSrcLine cmtStarters cmtStopers shortCmtStarters stringDelims
                acc srcLine =
    case firstMatchedTok srcLine cmtStarters cmtStopers shortCmtStarters
                         stringDelims of
      Nothing -> reverse acc
      Just (tok, remaining) -> tokenizeSrcLine
                                 cmtStarters cmtStopers shortCmtStarters
                                 stringDelims (tok:acc) remaining

-- file name to list of lines read
getLines :: String -> IO [String]
getLines fileName =
    do mfh <- try (openFile fileName ReadMode)
       case mfh of
         Left err -> do print err
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
tokenizeFile :: String -> [Token] -> [Token] -> [Token] -> [Token]
             -> IO [[LowLevelToken]]
tokenizeFile fileName cmtStarters cmtStopers shortCmtStarters stringDelims =
    do readLines <- getLines fileName
       return (map (tokenizeSrcLine cmtStarters cmtStopers shortCmtStarters
                                    stringDelims [])
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
                  CmtOrCodeOrString _ ->
                      highLevelTokens
                         (toks:others) parserState
                         depth (factorize parserState tok lineAcc) acc
                  LineCmtMark _ ->
                      highLevelTokens
                         (toks:others) ReadingShortCmt
                         depth (factorize parserState tok lineAcc) acc
                  CmtBegin _ ->
                      highLevelTokens
                         (toks:others) ReadingLongCmt
                         (depth+1) (factorize parserState tok lineAcc) acc
                  CmtEnd mark ->
                      error ("got EndCmtMArk while ReadingCode: " ++ mark)
                  StringBeginOrEnd _ ->
                      highLevelTokens
                         (toks:others) ReadingStringInCode
                         depth (factorize parserState tok lineAcc) acc
            ReadingShortCmt ->
                case tok of
                  StringBeginOrEnd _ ->
                      highLevelTokens
                         (toks:others) ReadingStringInShortCmt
                         depth (factorize parserState tok lineAcc) acc
                  _ -> highLevelTokens
                         (toks:others) ReadingShortCmt
                         depth (factorize parserState tok lineAcc) acc
            ReadingLongCmt ->
                case tok of
                  CmtEnd _ ->
                      let newDepth = depth - 1 in
                      if newDepth == 0 then
                          highLevelTokens
                             (toks:others) ReadingCode
                             newDepth (factorize parserState tok lineAcc) acc
                      else
                          highLevelTokens
                            (toks:others) parserState
                            newDepth (factorize parserState tok lineAcc) acc
                  CmtBegin _ ->
                      let newDepth = depth + 1 in
                      highLevelTokens
                         (toks:others) parserState
                         newDepth (factorize parserState tok lineAcc) acc
                  CmtOrCodeOrString _ ->
                      highLevelTokens
                         (toks:others) parserState
                         depth (factorize parserState tok lineAcc) acc
                  LineCmtMark mark ->
                      error ("got LineCmtMark while ReadingLongCmt: " ++ mark)
                  StringBeginOrEnd _ ->
                      highLevelTokens
                         (toks:others) ReadingStringInLongCmt
                         depth (factorize parserState tok lineAcc) acc
            ReadingStringInLongCmt ->
                case tok of
                  StringBeginOrEnd _ ->
                      highLevelTokens
                         (toks:others) ReadingLongCmt
                         depth (factorize parserState tok lineAcc) acc
                  _ ->
                      highLevelTokens
                         (toks:others) parserState
                         depth (factorize parserState tok lineAcc) acc
            ReadingStringInShortCmt ->
                case tok of
                  StringBeginOrEnd _ ->
                      highLevelTokens
                         (toks:others) ReadingShortCmt
                         depth (factorize parserState tok lineAcc) acc
                  _ ->
                      highLevelTokens
                         (toks:others) parserState
                         depth (factorize parserState tok lineAcc) acc
            ReadingStringInCode ->
                case tok of
                  StringBeginOrEnd _ ->
                      highLevelTokens
                         (toks:others) ReadingCode
                         depth (factorize parserState tok lineAcc) acc
                  _ ->
                      highLevelTokens
                         (toks:others) parserState
                         depth (factorize parserState tok lineAcc) acc

factorize :: ParserState -> LowLevelToken -> [HighLevelToken]
          -> [HighLevelToken]
factorize state elt [] = (promote state elt):[]
factorize state elt lst@(x:xs) =
    case elt of
      CmtOrCodeOrString _ ->
          case x of
            Code c -> (Code (c ++ (lltToString elt))):xs
            _      -> (promote state elt):lst
      CmtBegin _ ->
          case x of
            LongCmt l -> (LongCmt (l ++ (lltToString elt))):xs
            _         -> (promote state elt):lst
      CmtEnd _ ->
          case x of
            LongCmt l -> (LongCmt (l ++ (lltToString elt))):xs
            _         -> (promote state elt):lst
      LineCmtMark _ ->
          case x of
            ShortCmt s -> (ShortCmt (s ++ (lltToString elt))):xs
            _          -> (promote state elt):lst
      StringBeginOrEnd _ ->
          case x of
            StringInCode     s -> (StringInCode
                                   (s ++ (lltToString elt))):xs
            StringInShortCmt s -> (StringInShortCmt
                                   (s ++ (lltToString elt))):xs
            StringInLongCmt  s -> (StringInLongCmt
                                   (s ++ (lltToString elt))):xs
            _ -> (promote state elt):lst

rmCmtsWrapper :: String -> IO [[HighLevelToken]]
rmCmtsWrapper fileName =
    do lowLevelToks <- tokenizeFile fileName [] [] ["--"] ["\""]
       let highLevelToks = highLevelTokens lowLevelToks ReadingCode 0 [] []
           codeOnly = removeComments highLevelToks
       return codeOnly
    where
      removeComments :: [[HighLevelToken]] -> [[HighLevelToken]]
      removeComments srcLine = map (filter isCode) srcLine

      isCode :: HighLevelToken -> Bool
      isCode (Code _)         = True
      isCode (StringInCode _) = True
      isCode _                = False

lowLevelTokenizeWrapper :: String -> IO [[LowLevelToken]]
lowLevelTokenizeWrapper fileName =
    do lowLevelToks <- tokenizeFile fileName [] [] ["--"] ["\""]
       return lowLevelToks

highLevelTokenizeWrapper :: String -> IO [[HighLevelToken]]
highLevelTokenizeWrapper fileName =
    do lowLevelToks <- tokenizeFile fileName [] [] ["--"] ["\""]
       let highLevelToks = highLevelTokens lowLevelToks ReadingCode 0 [] []
       return highLevelToks

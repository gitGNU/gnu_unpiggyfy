module Main where

import Data.List          (intersperse, isPrefixOf)
import Data.Maybe         (Maybe)
import System.Environment (getArgs)
import IO                 (hPutStrLn, stderr)

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
parseCommandLine [] (Just lg) (Just filename) (Just mode) =
    Right (Action mode (lg, filename))
parseCommandLine [] lg filename mode =
    Left (errToStr (lg,filename,mode))
parseCommandLine (arg:args) lg filename mode =
    case arg of
      "-rc"  ->   parseCommandLine args lg filename (Just RemoveComments)
      "-llt" ->   parseCommandLine args lg filename (Just TokenizeLowLevel)
      "-hlt" ->   parseCommandLine args lg filename (Just TokenizeHighLevel)
      "-tc"  ->   parseCommandLine args lg filename (Just TokenizeCode)
      "-cc"  ->   parseCommandLine args lg filename (Just CompressCode)
      "-h"     -> Right Help
      "--help" -> Right Help
      _        -> let langPrfx  = "-lg:"
                      inputPrfx = "-i:" in
                  if isPrefixOf langPrfx arg then
                      case findLanguage (drop (length langPrfx) arg) of
                        Left errMsg -> Left errMsg
                        Right lang  -> parseCommandLine args (Just lang)
                                                        filename mode
                  else if isPrefixOf inputPrfx arg then
                      case maybeFilename (drop (length inputPrfx) arg) of
                        Nothing              -> Left "empty filename"
                        file@(Just fileName) -> parseCommandLine args lg
                                                                 file mode
                  else Right Help

errToStr :: (Maybe SupportedLanguage, Maybe FilePath, Maybe Mode) -> String
errToStr (Nothing,Nothing,Nothing) = "no language, filename and " ++
                                     "mode provided"
errToStr (Just _ ,Nothing,Nothing) = "no filename and mode provided"
errToStr (Nothing,Just _, Nothing) = "no language and mode provided"
errToStr (Just _ ,Just _, Nothing) = "no mode provided"
errToStr (Nothing,Nothing,Just _)  = "no language and filename provided"
errToStr (Just _ ,Nothing,Just _)  = "no filename provided"
errToStr (Nothing,Just _, Just _)  = "no language provided"

usage :: IO ()
usage = (error . concat . (intersperse "\n"))
            ["usage:"
            ," -h | -i:FILE -lg:{ C | HS } -{ rc | llt | hlt | tc | cc }"
            ," -h, --help -> what you are reading now"
            ," -i:FILE    -> source file to read from"
            ," -lg:{C,HS} -> your source code's language, HS is Haskell"
            ,"   -rc      -> remove comments mode"
            ,"   -llt     -> low level tokenization mode"
            ,"   -hlt     -> high level tokenization mode"
            ,"   -tc      -> tokenize code mode"
            ,"   -cc      -> compress code mode"]

main :: IO ()
main =
    do args <- getArgs
--     mapM_ print args -- uncomment to inspect command line
       case parseCommandLine args Nothing Nothing Nothing of
         Left errMsg -> do hPutStrLn stderr ("unpig: error: " ++ errMsg)
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

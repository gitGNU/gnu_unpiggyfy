module Languages (
    SupportedLanguage(..),
    Token,
    LanguageTags,
    ParseInfo,
    findLanguage,
    haskellParseInfo,
    cParseInfo,
) where

import Data.List

data SupportedLanguage = C
                       | Haskell
                       deriving Show

-- what programming language the user want us to analyze
findLanguage :: String -> Either String SupportedLanguage
findLanguage s | s == "C"  = Right C
               | s == "HS" = Right Haskell
               | otherwise = Left ("unsupported language: " ++ s)

-- a tuple of ([long comment starters],[long comment stoppers]
--            ,[short comment starters]
--            ,[string delimiters]
--            ,[escape characters])
type Token = String
type LanguageTags = ([Token],[Token],[Token],[Token],[Token])

type ParseInfo = (LanguageTags, [String], [String])

haskellTags :: LanguageTags
haskellTags = (["{-"],["-}"],["--"],["\""],["\\"])

-- special keywords can be glued together with non keywords
haskellSpecialKwds :: [String]
haskellSpecialKwds =
    (reverse . uniq)
    ["_","(",")","[","]",",","/=","<","<=","==",">",">=","."
    ,"||","|","&&","&","=","!","@","::",":","~","<-","->"
    ,"+","++","*","**","-","^","^^"]

haskellStandardKwds :: [String]
haskellStandardKwds =
    (reverse . uniq)
    ["as","case","of"
    ,"class","data","default","deriving","do","forall","foreign"
    ,"hiding","if","then","else","import","infix","infixl"
    ,"infixr","instance","let","in","mdo","module","newtype"
    ,"qualified","type","where"]

haskellParseInfo :: ParseInfo
haskellParseInfo = (haskellTags, haskellSpecialKwds, haskellStandardKwds)

cTags :: LanguageTags
cTags = (["/*"],["*/"],["//"],["\"","\'"],["\\"])

-- C99 keywords without GNU extension
cStandardKwds :: [String]
cStandardKwds =
    (reverse . uniq)
    ["auto","break","case","char","const","continue","default","do","double"
    ,"else","enum","extern","float","for","goto","if","int","long","register"
    ,"return","short","signed","sizeof","static","struct","switch","typedef"
    ,"union","unsigned","void","volatile","while"]

-- what I call special keywords seem to be called
-- "punctuators, operators, and preprocessing tokens" in the litterature
cSpecialKwds :: [String]
cSpecialKwds =
    (reverse . uniq)
    ["[","]","(",")","{","}",",",":",";","*","=","...","#",".","->","++","--"
    ,"##","&","+","-","~","!","/","%","<<",">>","!=","<",">","<=",">=","=="
    ,"^","|","&&","||","?","*=","/=","%=","+=","-=","<<=",">>=","&=","^=","|="
    ,"<:",":>","<%","%>","%:","%:%:"]

cParseInfo :: ParseInfo
cParseInfo = (cTags, cSpecialKwds, cStandardKwds)

-- remove duplicates
uniq :: Ord a => [a] -> [a]
uniq l = uniq' (sort l) []
    where uniq' :: Ord a => [a] -> [a] -> [a]
          uniq' []     acc = reverse acc
          uniq' (x:xs) acc = case xs of
                               [] -> uniq' xs (x:acc)
                               (x':_) | x == x' -> uniq' xs acc
                                      | otherwise -> uniq' xs (x:acc)

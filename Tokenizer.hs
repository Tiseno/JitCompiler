module Tokenizer
  ( Token(..)
  , tokenize
  , nameOf
  ) where

import qualified Data.Char as Char

data Token
  = TId String
  | TOpenParen
  | TClosingParen
  | TLambda String
  | TArrow
  | TLet
  | TEqual
  | TIn
  | TIntLit Int
  | TBoolLit Bool
  | TIf
  | TThen
  | TElse
  deriving (Show, Eq)

nameOf :: Token -> String
nameOf (TId _)       = "identifier"
nameOf TOpenParen    = "'('"
nameOf TClosingParen = "')'"
nameOf (TLambda _)   = "function"
nameOf TArrow        = "'->'"
nameOf TLet          = "let"
nameOf TEqual        = "'='"
nameOf TIn           = "in"
nameOf (TIntLit _)   = "int literal"
nameOf (TBoolLit _)  = "bool literal"
nameOf TIf           = "if"
nameOf TThen         = "then"
nameOf TElse         = "else"

isSpecialChar :: Char -> Bool
isSpecialChar c = c `elem` "|&<>=+-/*"

tokenize :: String -> Either String [Token]
tokenize ('\n':xs) = tokenize xs
tokenize (' ':xs) = tokenize xs
tokenize ('(':xs) = tokenize xs >>= (\ts -> pure $ TOpenParen : ts)
tokenize (')':xs) = tokenize xs >>= (\ts -> pure $ TClosingParen : ts)
tokenize ('\\':xs) =
  case xs of
    (x:_)
      | Char.isAlpha x -> do
        let (id, rest) = span Char.isAlphaNum xs
        ts <- tokenize rest
        pure $ TLambda id : ts
    (x:_) -> Left $ "Expected identifier after \\ but found '" ++ [x] ++ "'"
    [] -> Left "Expected identifier after \\ but reached end of input"
tokenize input@(x:xs)
  | Char.isAlpha x = do
    let (id, rest) = span Char.isAlphaNum input
    ts <- tokenize rest
    pure $
      (case id of
         "let"   -> TLet
         "in"    -> TIn
         "if"    -> TIf
         "then"  -> TThen
         "else"  -> TElse
         "True"  -> TBoolLit True
         "False" -> TBoolLit False
         _       -> TId id) :
      ts
tokenize input@(x:xs)
  | isSpecialChar x =
    let (id, rest) = span isSpecialChar input
     in if id == "--"
          then tokenize $ dropWhile (/= '\n') rest
          else do
            ts <- tokenize rest
            pure
              (case id of
                 "->" -> TArrow : ts
                 "="  -> TEqual : ts
                 _    -> TId id : ts)
tokenize input@(x:xs)
  | Char.isDigit x = do
    let (i, rest) = span Char.isDigit input
    ts <- tokenize rest
    pure $ TIntLit (read i) : ts
tokenize (x:_) = Left $ "Unrecognized '" ++ [x] ++ "'"
tokenize [] = pure []

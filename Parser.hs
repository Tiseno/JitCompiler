{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Parser
  ( parseProgram
  ) where

import           Expr      (Expr (..))
import           Tokenizer (Token (..), nameOf)

type Name = String

type PExpr = Expr ()

type ParseResult a = Either String ([Token], a)

newtype Parser a =
  Parser
    { runParser :: [Token] -> ParseResult a
    }

instance Functor Parser where
  fmap f (Parser parse) =
    Parser $ \input0 -> do
      (input1, a) <- parse input0
      pure (input1, f a)

instance Applicative Parser where
  pure a = Parser $ \input -> pure (input, a)
  (Parser p0) <*> (Parser p1) =
    Parser $ \input0 -> do
      (input1, f) <- p0 input0
      (input2, a) <- p1 input1
      pure (input2, f a)

instance Monad Parser where
  (Parser p0) >>= f =
    Parser $ \input0 -> do
      (input1, a) <- p0 input0
      runParser (f a) input1

expectedButFoundError t x =
  Left $ "Expected " ++ nameOf t ++ " but found " ++ nameOf x

endOfInputError t =
  Left $ "Expected " ++ nameOf t ++ " but reached end of input"

eat :: Token -> Parser ()
eat t =
  Parser $ \case
    x:xs
      | x == t -> Right (xs, ())
    x:_ -> expectedButFoundError t x
    [] -> endOfInputError t

parseLambda :: Parser Name
parseLambda =
  Parser $ \case
    TLambda n:xs -> Right (xs, n)
    x:_          -> expectedButFoundError (TLambda "") x
    []           -> endOfInputError (TLambda "")

parseApp :: Parser PExpr
parseApp = do
  eat TOpenParen
  e0 <- parseExpr
  e1 <- parseExpr
  eat TClosingParen
  pure (App e0 e1)

parseAbs :: Parser PExpr
parseAbs = do
  id <- parseLambda
  eat TArrow
  e <- parseExpr
  pure (Abs (id, ()) e)

parseId :: Parser Name
parseId =
  Parser $ \case
    TId id:xs -> Right (xs, id)
    x:_       -> expectedButFoundError (TId "") x
    []        -> endOfInputError (TId "")

parseLet :: Parser PExpr
parseLet = do
  eat TLet
  id <- parseId
  eat TEqual
  e0 <- parseExpr
  eat TIn
  e1 <- parseExpr
  pure (Let id e0 e1)

parseIf :: Parser PExpr
parseIf = do
  eat TIf
  cond <- parseExpr
  eat TThen
  e0 <- parseExpr
  eat TElse
  e1 <- parseExpr
  pure (If cond e0 e1)

parseExpr :: Parser PExpr
parseExpr =
  Parser $ \input ->
    case input of
      (TId n:xs)      -> Right (xs, Id n ())
      (TOpenParen:xs) -> runParser parseApp input
      (TLambda _:xs)  -> runParser parseAbs input
      (TLet:xs)       -> runParser parseLet input
      (TIntLit i:xs)  -> Right (xs, Const i)
      (TBoolLit b:xs) -> Right (xs, ConstBool b)
      (TIf:xs)        -> runParser parseIf input
      (t:_)           -> Left $ "Expected expression but found " ++ nameOf t
      []              -> Left "Expected expression but reached end of input"

parseProgram :: [Token] -> Either String PExpr
parseProgram tokens = fmap snd $ runParser parseExpr tokens

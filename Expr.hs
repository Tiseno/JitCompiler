module Expr
  ( Expr(..)
  , Pretty(..)
  ) where

import qualified Data.Char as Char

class Pretty a where
  pretty :: Int -> a -> String

type Name = String

data Expr a
  = Id Name a
  | App (Expr a) (Expr a)
  | Abs (Name, a) (Expr a)
  | Let Name (Expr a) (Expr a)
  | Const Int
  | ConstBool Bool
  | If (Expr a) (Expr a) (Expr a)
  deriving (Show)

instance Functor Expr where
  fmap f (Id x a)       = Id x (f a)
  fmap f (App e0 e1)    = App (fmap f e0) (fmap f e1)
  fmap f (Abs (x, a) e) = Abs (x, f a) (fmap f e)
  fmap f (Let x e0 e1)  = Let x (fmap f e0) (fmap f e1)
  fmap f (Const i)      = Const i
  fmap f (ConstBool b)  = ConstBool b
  fmap f (If c e0 e1)   = If (fmap f c) (fmap f e0) (fmap f e1)

instance Pretty a => Pretty (Expr a) where
  pretty :: Pretty a => Int -> Expr a -> String
  pretty _ (Id x t)
    | any Char.isAlpha x = x ++ pretty 0 t
    | otherwise = "(" ++ x ++ ")" ++ pretty 0 t
  pretty i (App e0 e1) = "(" ++ pretty i e0 ++ " " ++ pretty i e1 ++ ")"
  pretty i (Abs (x, t) e) = "\\" ++ x ++ pretty i t ++ " -> " ++ pretty i e
  pretty i (Let x e0 e1) =
    let indent = replicate (2 * i) ' '
        i' = i + 1
     in "\n" ++
        indent ++
        "let " ++
        x ++ " = " ++ pretty i' e0 ++ "\n" ++ indent ++ "in " ++ pretty i' e1
  pretty _ (Const i) = show i
  pretty _ (ConstBool b) = show b
  pretty i (If cond e0 e1) =
    let indent = replicate (2 * i) ' '
        i' = i + 1
     in "\n" ++
        indent ++
        "if " ++
        pretty i' cond ++
        "\n" ++
        indent ++
        "then " ++ pretty i' e0 ++ "\n" ++ indent ++ "else " ++ pretty i' e1

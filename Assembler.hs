module Assembler
  ( AExpr(..)
  , AType(..)
  , makeProgram
  ) where

import           Checker                   (CExpr, CType (..))
import           Control.Monad.Trans.State
import qualified Data.Bifunctor            as Bifunctor
import qualified Data.Map                  as Map
import           Expr
import           Machine                   (Adr, IP, Instruction (..),
                                            MachineResult, MachineState (..),
                                            run, showMachineResult,
                                            tagPrimitiveFnAdd,
                                            tagPrimitiveFnAnd,
                                            tagPrimitiveFnDiv,
                                            tagPrimitiveFnEquals,
                                            tagPrimitiveFnGreaterThan,
                                            tagPrimitiveFnLessThan,
                                            tagPrimitiveFnMul, tagPrimitiveFnOr,
                                            tagPrimitiveFnSub, tagStaticFn)
import qualified Text.Read                 as Text

data AType
  = AInt
  | ABool
  | AFn AType AType
  deriving (Show)

type AExpr = Expr AType

typeOf :: AExpr -> AType
typeOf (Let _ _ e) = typeOf e
typeOf (App e _) = returnTypeOf $ typeOf e
  where
    returnTypeOf (AFn _ t) = t
typeOf (Abs (x, t) e) = AFn t (typeOf e)
typeOf (Id _ t) = t
typeOf (Const _) = AInt

data Label =
  Label String Int Bool
  deriving (Show, Eq, Ord)

data Asm
  = AInstr Instruction
  | ALebel Instruction Label
  | AStore Label
  | ALoad Label
  | APushStaticRef Label
  | AJmpLabel Label
  | AJmp Label
  | AJmpIf Label
  deriving (Show)

type StackSize = Int

type Ref = (Label, StackEffect)

type Ctx = Map.Map String Ref

type Statics = [(Label, [Asm])]

type AssembleState = (Int, Statics)

getRefInScope :: Ctx -> String -> Ref
getRefInScope ctx x =
  case Map.lookup x ctx of
    Nothing  -> error $ "Could not find " ++ show x ++ " in scope"
    Just ref -> ref

newLabelFor :: Bool -> String -> State AssembleState Label
newLabelFor dyn x = do
  (c, s) <- get
  put (c + 1, s)
  pure $ Label x c dyn

newStaticLabelFor :: String -> State AssembleState Label
newStaticLabelFor = newLabelFor False

newDynamicLabelFor :: String -> State AssembleState Label
newDynamicLabelFor = newLabelFor True

saveStatic :: (Label, [Asm]) -> State AssembleState ()
saveStatic x = do
  (c, s) <- get
  put (c, x : s)

stackEffectOfType :: AType -> StackEffect
stackEffectOfType ABool     = StackPrimitive
stackEffectOfType AInt      = StackPrimitive
stackEffectOfType (AFn _ _) = StackFunctionRef

stackEffectOfFnType (AFn _ r) = stackEffectOfType r
stackEffectOfFnType t =
  error $ "Tried to get function stack effect non function " ++ show t

sizeOf :: StackEffect -> Int
sizeOf UnknownStackEffect = error "Cannot get size of an unknown stack effect"
sizeOf StackPrimitive     = 1
sizeOf StackFunctionRef   = 2

data StackEffect
  = UnknownStackEffect
  | StackPrimitive
  | StackFunctionRef

optimize asm = optimizeLoadSwap $ optimizeLoad $ optimizeDup asm

-- This will only work for loading primitives
optimizeDup ((ALoad l1):(ALoad l2):xs)
  | l1 == l2 = ALoad l2 : AInstr Dup : optimizeDup xs
optimizeDup (x:xs) = x : optimizeDup xs
optimizeDup [] = []

-- This does not work if we load it again afterwards (it will not get stored)
optimizeLoad :: [Asm] -> [Asm]
optimizeLoad ((AInstr (Push _)):(AStore l1):(ALoad l2):xs)
  | l1 == l2 = optimizeLoad xs
optimizeLoad (x:xs) = x : optimizeLoad xs
optimizeLoad [] = []

-- This does not work if we load it again afterwards (it will not get stored)
optimizeLoadSwap :: [Asm] -> [Asm]
optimizeLoadSwap (p1@(AInstr (Push _)):p2@(AInstr (Push _)):(AInstr (Push 2)):(AStore l1):p3@(AInstr (Push _)):(ALoad l2):xs)
  | l1 == l2 = p3 : p1 : p2 : optimizeLoadSwap xs
optimizeLoadSwap (p1@(AInstr (Push _)):(AInstr (Push 1)):(AStore l1):p2@(AInstr (Push _)):(ALoad l2):xs)
  | l1 == l2 = p1 : p2 : optimizeLoadSwap xs
optimizeLoadSwap (x:xs) = x : optimizeLoadSwap xs
optimizeLoadSwap [] = []

assemble :: Ctx -> AExpr -> State AssembleState (StackEffect, [Asm])
assemble ctx (Const i) = pure (StackPrimitive, [AInstr $ Push i])
assemble ctx (App (App (Id "&&" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackPrimitive, asm2 ++ asm1 ++ [AInstr And])
assemble ctx (App (App (Id "||" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackPrimitive, asm2 ++ asm1 ++ [AInstr Or])
assemble ctx (App (App (Id "==" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackPrimitive, asm2 ++ asm1 ++ [AInstr Equals])
assemble ctx (App (App (Id "<" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackPrimitive, asm2 ++ asm1 ++ [AInstr LessThan])
assemble ctx (App (App (Id ">" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackPrimitive, asm2 ++ asm1 ++ [AInstr GreaterThan])
assemble ctx (App (App (Id "+" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackPrimitive, asm2 ++ asm1 ++ [AInstr Add])
assemble ctx (App (App (Id "-" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackPrimitive, asm2 ++ asm1 ++ [AInstr Sub])
assemble ctx (App (App (Id "*" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackPrimitive, asm2 ++ asm1 ++ [AInstr Mul])
assemble ctx (App (App (Id "/" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackPrimitive, asm2 ++ asm1 ++ [AInstr Div])
assemble ctx (App (Id "&&" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnAnd])
assemble ctx (App (Id "||" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnOr])
assemble ctx (App (Id "==" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnEquals])
assemble ctx (App (Id "<" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnLessThan])
assemble ctx (App (Id ">" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnGreaterThan])
assemble ctx (App (Id "+" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnAdd])
assemble ctx (App (Id "-" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnSub])
assemble ctx (App (Id "*" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnMul])
assemble ctx (App (Id "/" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnDiv])
assemble ctx (App e1 e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  let se = stackEffectOfFnType $ typeOf e1
  pure (se, asm2 ++ asm1 ++ [AInstr CallDynamic])
assemble ctx (Id x t) = do
  let (label, _) = getRefInScope ctx x
  let se = stackEffectOfType t
  pure (se, [ALoad label])
assemble ctx (Let x e1 e2) = do
  label <- newStaticLabelFor x
  let newCtx = Map.insert x (label, UnknownStackEffect) ctx
  (e1se, asm1) <- assemble newCtx e1
  let newCtx2 = Map.insert x (label, e1se) ctx
  (e2se, asm2) <- assemble newCtx2 e2
  pure (e2se, asm1 ++ [AInstr $ Push (sizeOf e1se), AStore label] ++ asm2)
assemble ctx (Abs (x, t) e) = do
  label <- newDynamicLabelFor x
  let xSize = stackEffectOfType t
  let newCtx = Map.insert x (label, xSize) ctx
  (_, asm) <- assemble newCtx e
  let static =
        [AInstr $ Push (sizeOf xSize), AStore label] ++ asm ++ [AInstr Ret]
  ref <- newStaticLabelFor $ "abs:" ++ x
  saveStatic (ref, static)
  pure (StackFunctionRef, [APushStaticRef ref, AInstr $ Push tagStaticFn])
assemble ctx (If cond true false) = do
  (_, condAsm) <- assemble ctx cond
  (_, trueAsm) <- assemble ctx true
  trueLabel <- newStaticLabelFor "jump"
  (se, falseAsm) <- assemble ctx false
  endLabel <- newStaticLabelFor "end"
  pure
    ( se
    , condAsm ++
      [AJmpIf trueLabel] ++
      falseAsm ++
      [AJmp endLabel, AJmpLabel trueLabel] ++ trueAsm ++ [AJmpLabel endLabel])

type LabelAdrMapping = Map.Map Label Adr

getAdr :: LabelAdrMapping -> Label -> Int
getAdr map l =
  case Map.lookup l map of
    Nothing  -> error $ "Could not find " ++ show l ++ " in adress mapping"
    Just adr -> adr

toInstruction :: LabelAdrMapping -> Asm -> [Instruction]
toInstruction _ (AInstr i)                  = [i]
toInstruction _ (AStore (Label _ i True))   = [StoreDynamic i]
toInstruction _ (ALoad (Label _ i True))    = [LoadDynamic i]
toInstruction _ (AStore (Label _ i False))  = [Store i]
toInstruction _ (ALoad (Label _ i False))   = [Load i]
toInstruction adrMapping (APushStaticRef l) = [Push (getAdr adrMapping l)]
toInstruction _ (AJmpLabel l)               = [Noop] -- TODO we can prevent this by counting real instructions
toInstruction adrMapping (AJmpIf l)         = [JmpIf (getAdr adrMapping l)]

makeProgram expr =
  let ((_, asm), (_, staticCode0)) =
        runState (assemble Map.empty expr) (1000000, [])
      mainCode0 = asm ++ [AInstr Exit]
      mainCode = mainCode0
      staticCode = staticCode0
      labelMapping = collectStaticLabels Map.empty mainCode staticCode
      code = mainCode ++ concatMap snd staticCode
      labelMapping' = collectJmpLabels labelMapping code
      codeWithJmpLabels = assignJmpLabels labelMapping' code
      program = concatMap (toInstruction labelMapping') codeWithJmpLabels
   in program
  where
    collectStaticLabels ::
         LabelAdrMapping -> [Asm] -> [(Label, [Asm])] -> LabelAdrMapping
    collectStaticLabels mapping mainCode staticCode =
      snd $ foldl f (length mainCode, mapping) staticCode
      where
        f :: (IP, LabelAdrMapping) -> (Label, [Asm]) -> (IP, LabelAdrMapping)
        f (ip, mapping) (l, asm) = (ip + length asm, Map.insert l ip mapping)
    collectJmpLabels :: LabelAdrMapping -> [Asm] -> LabelAdrMapping
    collectJmpLabels mapping asm = foldl f mapping $ zip [0 ..] asm
      where
        f mapping (i, AJmpLabel l) = Map.insert l i mapping
        f mapping _                = mapping
    assignJmpLabels :: LabelAdrMapping -> [Asm] -> [Asm]
    assignJmpLabels mapping = fmap (f mapping)
      where
        f mapping (AJmp l)   = AInstr $ Jmp $ mapping Map.! l
        f mapping (AJmpIf l) = AInstr $ JmpIf $ mapping Map.! l
        f mapping a          = a

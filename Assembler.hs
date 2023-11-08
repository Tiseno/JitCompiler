module Assembler
  ( Expr(..)
  , EType(..)
  , makeProgram
  ) where

import           Control.Monad.Trans.State
import qualified Data.Array                as Array
import qualified Data.Bifunctor            as Bifunctor
import qualified Data.Char                 as Char
import qualified Data.Map                  as Map
import qualified Debug.Trace               as Debug
import           Machine                   (Adr, IP, Instruction (..),
                                            MachineResult, MachineState (..),
                                            run, showMachineResult,
                                            tagPrimitiveFnAdd,
                                            tagPrimitiveFnMul,
                                            tagPrimitiveFnSub, tagStaticFn)
import qualified Text.Read                 as Text

type Name = String

-- TODO why do I even have types and stack effects as separate things?
-- A EInt should always be 1 and a function should always be 2 (primitive, static, or dynamic)
-- - Because of thunks, a EInt can actually be a closure
data EType
  = EInt
  | EBool
  | EFn EType EType
  deriving (Show)

data Expr
  = Let Name Expr Expr
  | App Expr Expr
  | Abs (Name, EType) Expr
  | Id Name EType
  | Const Int
  | If Expr Expr Expr
  deriving (Show)

typeOf :: Expr -> EType
typeOf (Let _ _ e) = typeOf e
typeOf (App e _) = returnTypeOf $ typeOf e
  where
    returnTypeOf (EFn _ t) = t
typeOf (Abs (x, t) e) = EFn t (typeOf e)
typeOf (Id _ t) = t
typeOf (Const _) = EInt

data Label =
  Label String Bool
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
  let l = x ++ "." ++ show c
  pure $ Label l dyn

newStaticLabelFor :: String -> State AssembleState Label
newStaticLabelFor = newLabelFor False

newDynamicLabelFor :: String -> State AssembleState Label
newDynamicLabelFor = newLabelFor True

saveStatic :: (Label, [Asm]) -> State AssembleState ()
saveStatic x = do
  (c, s) <- get
  put (c, x : s)

stackEffectOfType :: EType -> StackEffect
stackEffectOfType EBool     = StackBool
stackEffectOfType EInt      = StackInt
stackEffectOfType (EFn _ _) = StackFunctionRef

stackEffectOfFnType (EFn _ r) = stackEffectOfType r
stackEffectOfFnType t =
  error $ "Tried to get function stack effect non function " ++ show t

sizeOf :: StackEffect -> Int
sizeOf UnknownStackEffect = error "Cannot get size of an unknown stack effect"
sizeOf StackBool          = 1
sizeOf StackInt           = 1
sizeOf StackFunctionRef   = 2

data StackEffect
  = UnknownStackEffect
  | StackBool
  | StackInt
  | StackFunctionRef

optimize asm = optimizeLoadSwap $ optimizeLoad $ optimizeDup asm

optimizeDup ((ALoad l1):(ALoad l2):xs)
  | l1 == l2 = ALoad l2 : AInstr Dup : optimizeDup xs
optimizeDup (x:xs) = x : optimizeDup xs
optimizeDup [] = []

optimizeLoad :: [Asm] -> [Asm]
optimizeLoad ((AInstr (Push _)):(AStore l1):(ALoad l2):xs)
  | l1 == l2 = optimizeLoad xs
optimizeLoad (x:xs) = x : optimizeLoad xs
optimizeLoad [] = []

optimizeLoadSwap :: [Asm] -> [Asm]
optimizeLoadSwap (p1@(AInstr (Push _)):p2@(AInstr (Push _)):(AInstr (Push 2)):(AStore l1):p3@(AInstr (Push _)):(ALoad l2):xs)
  | l1 == l2 = p3 : p1 : p2 : optimizeLoadSwap xs
optimizeLoadSwap (p1@(AInstr (Push _)):(AInstr (Push 1)):(AStore l1):p2@(AInstr (Push _)):(ALoad l2):xs)
  | l1 == l2 = p1 : p2 : optimizeLoadSwap xs
optimizeLoadSwap (x:xs) = x : optimizeLoadSwap xs
optimizeLoadSwap [] = []

assemble :: Ctx -> Expr -> State AssembleState (StackEffect, [Asm])
assemble ctx (Const i) = pure (StackInt, [AInstr $ Push i])
assemble ctx (App (App (Id "==" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackBool, asm2 ++ asm1 ++ [AInstr Equals])
assemble ctx (App (App (Id "<" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackBool, asm2 ++ asm1 ++ [AInstr LessThan])
-- TODO other comparisons and partial comparisn closures
assemble ctx (App (App (Id "+" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackInt, asm2 ++ asm1 ++ [AInstr Add])
assemble ctx (App (App (Id "-" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackInt, asm2 ++ asm1 ++ [AInstr Sub])
assemble ctx (App (App (Id "*" _) e1) e2) = do
  (_, asm2) <- assemble ctx e2
  (_, asm1) <- assemble ctx e1
  pure (StackInt, asm2 ++ asm1 ++ [AInstr Mul])
assemble ctx (App (Id "+" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnAdd])
assemble ctx (App (Id "-" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnSub])
assemble ctx (App (Id "*" _) e) = do
  (_, asm) <- assemble ctx e
  pure (StackFunctionRef, asm ++ [AInstr $ Push tagPrimitiveFnMul])
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
  (_, asm) <- assemble newCtx e -- TODO We do not need to save this stack effect as we know what effect the function will have because of typing
  let static =
        [AInstr $ Push (sizeOf xSize), AStore label] ++ asm ++ [AInstr Ret]
  ref <- newStaticLabelFor "abs"
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

readLabel :: Label -> Int
readLabel (Label l dyn) =
  case Text.readMaybe $ drop 1 $ dropWhile (/= '.') l of
    Nothing -> error $ "Could not read label " ++ l
    Just i  -> i

type LabelAdrMapping = Map.Map Label Adr

getLabel :: LabelAdrMapping -> Label -> Int
getLabel map l =
  case Map.lookup l map of
    Nothing  -> error $ "Could not find " ++ show l ++ " in adress mapping"
    Just adr -> adr

toInstruction :: LabelAdrMapping -> Asm -> [Instruction]
toInstruction _ (AInstr i)                 = [i]
toInstruction _ (AStore l@(Label _ True))  = [StoreDynamic (readLabel l)]
toInstruction _ (ALoad l@(Label _ True))   = [LoadDynamic (readLabel l)]
toInstruction _ (AStore l@(Label _ False)) = [Store (readLabel l)]
toInstruction _ (ALoad l@(Label _ False))  = [Load (readLabel l)]
toInstruction labels (APushStaticRef l)    = [Push (getLabel labels l)]
toInstruction _ (AJmpLabel l)              = [Noop] -- TODO we can prevent this by counting real instructions
toInstruction labels (AJmpIf l)            = [JmpIf (getLabel labels l)]

makeProgram shouldOpt expr =
  let ((_, asm), (_, staticCode0)) =
        runState (assemble Map.empty expr) (1000000, [])
      mainCode0 = asm ++ [AInstr Exit]
      mainCode =
        if shouldOpt
          then optimize mainCode0
          else mainCode0
      staticCode =
        if shouldOpt
          then fmap (Bifunctor.second optimize) staticCode0
          else staticCode0
      labelMapping = collectStaticLabels Map.empty mainCode staticCode
      code = mainCode ++ concatMap snd staticCode
      labelMapping' = collectJmpLabels labelMapping code
      codeWithJmpLabels = assignJmpLabels labelMapping' code
      instructions = concatMap (toInstruction labelMapping') codeWithJmpLabels
   in (instructions, staticCode0)
  where
    collectStaticLabels ::
         LabelAdrMapping -> [Asm] -> [(Label, [Asm])] -> LabelAdrMapping
    collectStaticLabels mapping mainCode staticCode =
      snd $ foldl foldStaticLabels (length mainCode, mapping) staticCode
    foldStaticLabels ::
         (IP, LabelAdrMapping) -> (Label, [Asm]) -> (IP, LabelAdrMapping)
    foldStaticLabels (ip, mapping) (l, asm) =
      let offset = length asm
       in (ip + offset, Map.insert l ip mapping)
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

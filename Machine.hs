{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Machine
  ( Instruction(..)
  , MachineResult
  , stack
  , run
  ) where

import qualified Control.Monad as Monad
import qualified Debug.Trace   as Debug

data Instruction
  = Noop
  | Exit
  | Push Int
  | Pop
  | Add
  | Sub
  | Mul
  | LessThan
  | Equals
  | And
  | Or
  | Not
  | Dup
  | Dup2
  | Swap
  | Dig2
  | Dig3
  | Jmp IP
  | JmpIf IP
  | Call IP
  | Ret
  | DebugState String
  deriving (Show)

type IP = Int

type Instructions = [Instruction]

type Stack = [Int]

data MachineState =
  MachineState
    { instrs    :: Instructions
    , ip        :: IP
    , callStack :: [IP]
    , stack     :: Stack
    }
  deriving (Show)

type MachineResult a = Either (MachineState, Maybe String) (MachineState, a)

newtype Machine a =
  Machine
    { runMachine :: MachineState -> MachineResult a
    }

instance Functor Machine where
  fmap f m =
    Machine $ \st0 -> do
      (st1, a) <- runMachine m st0
      pure (st1, f a)

instance Applicative Machine where
  pure a = Machine $ \st0 -> pure (st0, a)
  m0 <*> m1 =
    Machine $ \st0 -> do
      (st1, f) <- runMachine m0 st0
      (st2, a) <- runMachine m1 st1
      pure (st2, f a)

instance Monad Machine where
  m0 >>= f =
    Machine $ \st0 -> do
      (st1, a) <- runMachine m0 st0
      let m1 = f a
      (st2, b) <- runMachine m1 st1
      pure (st2, b)

noop = Machine $ \st0 -> Right (st0, ())

exit = Machine $ \st0 -> Left (st0, Nothing)

push :: Int -> Machine ()
push i = Machine $ \st0 -> pure (st0 {stack = i : stack st0}, ())

pop :: Machine Int
pop =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      []     -> Left (st0, Just "Pop on empty stack")
      (x:xs) -> Right (st0 {stack = xs}, x)

swap :: Machine ()
swap =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:y:xs) -> Right (st0 {stack = y : x : xs}, ())
      _        -> Left (st0, Just "Swap on stack without 2 elements")

dig2 :: Machine ()
dig2 =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:y:n:xs) -> Right (st0 {stack = n : x : y : xs}, ())
      _          -> Left (st0, Just "Dig 2 on stack with fewer than 3 elements")

dig3 :: Machine ()
dig3 =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:y:z:n:xs) -> Right (st0 {stack = n : x : y : z : xs}, ())
      _ -> Left (st0, Just "Dig 3 on stack with fewer than 4 elements")

dup :: Machine ()
dup =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:xs) -> Right (st0 {stack = x : x : xs}, ())
      _      -> Left (st0, Just "Dup on empty stack")

dup2 :: Machine ()
dup2 =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:y:xs) -> Right (st0 {stack = x : y : x : y : xs}, ())
      _        -> Left (st0, Just "Dup on stack with fewer than 2 elements")

debugState :: String -> Machine ()
debugState s =
  Machine $ \st0@(MachineState {instrs, ip, stack}) ->
    Right
      ( st0
          { instrs =
              Debug.trace
                ("\nDebug " ++
                 s ++
                 "\ninstr: " ++ show (instrs !! ip) ++ "\nstack: " ++ show stack)
                instrs
          }
      , ())

pushCallStack :: Machine ()
pushCallStack =
  Machine $ \st0@(MachineState {ip, callStack}) ->
    Right (st0 {callStack = ip : callStack}, ())

popCallStack :: Machine IP
popCallStack =
  Machine $ \st0@(MachineState {callStack}) ->
    case callStack of
      []     -> Left (st0, Just "Pop on empty call stack")
      (x:xs) -> Right (st0 {callStack = xs}, x)

nextInstr :: Machine Instruction
nextInstr =
  Machine $ \st0@(MachineState {instrs, ip}) ->
    case elemAt ip instrs of
      Nothing    -> Left (st0, Just "IP out of bounds")
      Just instr -> Right (st0 {ip = ip + 1}, instr)
    --- | Why is this not in prelude?
  where
    elemAt :: Int -> [a] -> Maybe a
    elemAt i = elemAt' 0 i
      where
        elemAt' x i (e:es)
          | x == i = Just e
        elemAt' x i (e:es) = elemAt' (x + 1) i es
        elemAt' _ _ _ = Nothing

jmp :: IP -> Machine ()
jmp p =
  Machine $ \st0@(MachineState {instrs, ip}) ->
    if p >= length instrs
      then Left (st0, Just "IP out of bounds")
      else Right (st0 {ip = p}, ())

intOp :: (Int -> Int -> Int) -> Machine ()
intOp fn = do
  b <- pop
  a <- pop
  push $ fn a b

boolToInt True  = 1
boolToInt False = 0

intToBool 0 = False
intToBool _ = True

comparison :: (Int -> Int -> Bool) -> Machine ()
comparison fn = do
  b <- pop
  a <- pop
  push $ boolToInt $ fn a b

boolOp :: (Bool -> Bool -> Bool) -> Machine ()
boolOp fn = do
  b <- pop
  a <- pop
  push $ boolToInt $ fn (intToBool a) (intToBool b)

boolNot :: Machine ()
boolNot = do
  a <- pop
  push $ boolToInt $ not (intToBool a)

jmpIf :: IP -> Machine ()
jmpIf p = do
  b <- pop
  Monad.when (intToBool b) $ jmp p

call :: IP -> Machine ()
call p = do
  pushCallStack
  jmp p

ret :: Machine ()
ret = do
  ip <- popCallStack
  jmp ip

oneInstruction :: Machine ()
oneInstruction = do
  instr <- nextInstr
  case instr of
    Noop         -> noop
    Exit         -> exit
    Push i       -> push i
    Pop          -> Monad.void pop
    Add          -> intOp (+)
    Sub          -> intOp (-)
    Mul          -> intOp (*)
    LessThan     -> comparison (<)
    Equals       -> comparison (==)
    And          -> boolOp (&&)
    Or           -> boolOp (||)
    Not          -> boolNot
    Dup          -> dup
    Dup2         -> dup2
    Swap         -> swap
    Dig2         -> dig2
    Dig3         -> dig3
    Jmp p        -> jmp p
    JmpIf p      -> jmpIf p
    Call p       -> call p
    Ret          -> ret
    DebugState s -> debugState s

untilHalt :: Machine ()
untilHalt = loop
  where
    loop = oneInstruction >> loop

run prg = runMachine untilHalt (MachineState prg 0 [] [])

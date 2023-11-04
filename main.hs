{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import qualified Control.Monad as Monad
import qualified Debug.Trace   as Debug

--- | Why is this not in prelude?
elemAt :: Int -> [a] -> Maybe a
elemAt i = elemAt' 0 i
  where
    elemAt' x i (e:es)
      | x == i = Just e
    elemAt' x i (e:es) = elemAt' (x + 1) i es
    elemAt' _ _ _ = Nothing

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
    , stack     :: Stack
    , callStack :: [IP]
    }
  deriving (Show)

type MachineResult a = Either (MachineState, String) (MachineState, a)

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

exit msg = Machine $ \st0 -> Left (st0, msg)

push :: Int -> Machine ()
push i = Machine $ \st0 -> pure (st0 {stack = i : stack st0}, ())

pop :: Machine Int
pop =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      []     -> Left (st0, "Pop on empty stack")
      (x:xs) -> Right (st0 {stack = xs}, x)

swap :: Machine ()
swap =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:y:xs) -> Right (st0 {stack = y : x : xs}, ())
      _        -> Left (st0, "Swap on stack without 2 elements")

dig2 :: Machine ()
dig2 =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:y:n:xs) -> Right (st0 {stack = n : x : y : xs}, ())
      _ ->
        Left
          ( st0
          , "Dig 2 on stack with fewer than 3 elements (" ++ show stack ++ ")")

dig3 :: Machine ()
dig3 =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:y:z:n:xs) -> Right (st0 {stack = n : x : y : z : xs}, ())
      _ ->
        Left
          ( st0
          , "Dig 3 on stack with fewer than 4 elements (" ++ show stack ++ ")")

dup :: Machine ()
dup =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:xs) -> Right (st0 {stack = x : x : xs}, ())
      _      -> Left (st0, "Dup on empty stack")

dup2 :: Machine ()
dup2 =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:y:xs) -> Right (st0 {stack = x : y : x : y : xs}, ())
      _        -> Left (st0, "Dup on stack with fewer than 2 elements")

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
      []     -> Left (st0, "Pop on empty call stack")
      (x:xs) -> Right (st0 {callStack = xs}, x)

readInstr :: Machine Instruction
readInstr =
  Machine $ \st0@(MachineState {instrs, ip}) ->
    case elemAt ip instrs of
      Nothing    -> Left (st0, "IP out of bounds")
      Just instr -> Right (st0 {ip = ip + 1}, instr)

jmp :: IP -> Machine ()
jmp p =
  Machine $ \st0@(MachineState {instrs, ip}) ->
    if p >= length instrs
      then Left (st0, "IP out of bounds")
      else Right (st0 {ip = p}, ())

boolToInt True  = 1
boolToInt False = 0

intToBool 0 = False
intToBool _ = True

intOp :: (Int -> Int -> Int) -> Machine ()
intOp fn = do
  b <- pop
  a <- pop
  let r = fn a b
  Monad.when (r < 0) $ exit $ "Negative integer " ++ show r
  push $ fn a b

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
  instr <- readInstr
  case instr of
    Noop         -> noop
    Exit         -> exit "Exit"
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

fibonacciProgram =
  [ Push 0
  , Push 1
  , Push 0
  , Push 10
  -- #begin
  , Dup2
  , Equals
  , JmpIf 17 -- #done
  , Dig3
  , Dig3
  , Dup
  , Dig2
  , Add
  , Dig3
  , Push 1
  , Add
  , Dig3
  , Jmp 4 -- #begin
  -- #done
  , Pop
  , Pop
  , Swap
  , Pop
  , Exit
  ]

fib n = fib' n 0 1 0
  where
    fib' n i f1 _
      | n == i = f1
    fib' n i f1 f0 = fib' n (i + 1) (f1 + f0) f1

fibonacciProgram2 offset =
  [ Dup2 -- #begin 0
  , Equals
  , Not
  , JmpIf (9 + offset) -- #loop
  , Pop
  , Pop
  , Swap
  , Pop
  , Exit
  , Dig3 -- #loop 9
  , Dig3
  , Dup
  , Dig2
  , Add
  , Dig3
  , Push 1
  , Add
  , Dig3
  , Jmp (0 + offset) -- #begin
  ]

callFibProgram =
  let callingCode = [Push 0, Push 1, Push 0, Push 22, Call 6, Exit]
   in callingCode ++ fibonacciProgram2 (length callingCode)

fibProgram offset =
  let callingCode = [Push 0, Push 1, Push 0, Dig3, Call (6 + offset), Exit]
   in callingCode ++ fibonacciProgram2 (length callingCode + offset)

callNestedFibProgram =
  let callingCode = [Push 22, Call 3, Exit]
   in callingCode ++ fibProgram (length callingCode)

fibr n = fibr' n
  where
    fibr' 0 = 1
    fibr' 1 = 1
    fibr' n = fibr' (n - 1) + fibr' (n - 2)

-- [n] dup
-- [n n] push 2
-- [n n 2] lessthan
-- [n b] not
-- [n !b] jmpIf #next
-- [n] pop
-- [] push 1
-- [1] ret
-- #next
-- [n] dup
-- [n n] push 1
-- [n n 1] sub
-- [n (n-1)] call fibr'
-- [n f1] swap
-- [f1 n] push 2
-- [f1 n 2] sub
-- [f1 (n-2)] call fibr'
-- [f1 f2] add
-- [fn] ret
recFibProgram offset =
  [ Dup -- #begin 0
  , Push 2
  , LessThan
  , Not
  , JmpIf (8 + offset) -- #next
  , Pop
  , Push 1
  , Ret
  , Dup -- #next
  , Push 1
  , Sub
  , Call (0 + offset)
  , Swap
  , Push 2
  , Sub
  , Call (0 + offset)
  , Add
  , Ret
  ]

callRecFibProgram =
  let callingCode = [Push 22, Call 3, Exit]
   in callingCode ++ recFibProgram (length callingCode)

programs =
  [fibonacciProgram, callFibProgram, callNestedFibProgram, callRecFibProgram]

run prg = runMachine untilHalt (MachineState prg 0 [] [])

showResult :: MachineResult () -> String
showResult mr =
  case mr of
    Left (ms, msg) -> show (stack ms) ++ " " ++ msg
    Right (ms, _)  -> show $ stack ms

main = do
  mapM_ (putStrLn . showResult . run) programs

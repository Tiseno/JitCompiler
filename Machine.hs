module Machine
  ( Instruction(..)
  , MachineResult
  , MachineState(..)
  , showMachineResult
  , IP
  , Adr
  , run
  , tagStaticFn
  , tagDynamicFn
  , tagPrimitiveFnAnd
  , tagPrimitiveFnOr
  , tagPrimitiveFnEquals
  , tagPrimitiveFnLessThan
  , tagPrimitiveFnGreaterThan
  , tagPrimitiveFnAdd
  , tagPrimitiveFnSub
  , tagPrimitiveFnMul
  , tagPrimitiveFnDiv
  ) where

import qualified Control.Monad as Monad
import qualified Data.Array    as Array
import qualified Data.Map      as Map
import qualified Debug.Trace   as Debug

type IP = Int

type Adr = Int

data Instruction
  = Noop
  | Exit
  | Push Int
  | Pop
  | Swap
  | Dup
  | Dup2
  | Dig2
  | Dig3
  | Not
  | And
  | Or
  | LessThan
  | GreaterThan
  | Equals
  | Add
  | Sub
  | Mul
  | Div
  | Jmp IP
  | JmpIf IP
  | Store Adr
  | Load Adr
  | StoreDynamic Adr
  | LoadDynamic Adr
  | Call
  | CallDynamic
  | Ret
  deriving (Show)

type Instructions = Array.Array IP Instruction

type Stack = [Int]

tagStaticFn = 11111

tagDynamicFn = 11112

tagPrimitiveFnAnd = 11113

tagPrimitiveFnOr = 11114

tagPrimitiveFnEquals = 11115

tagPrimitiveFnLessThan = 11116

tagPrimitiveFnGreaterThan = 11117

tagPrimitiveFnAdd = 11118

tagPrimitiveFnSub = 11119

tagPrimitiveFnMul = 11120

tagPrimitiveFnDiv = 11120

type HeapValue = [Int]

type Heap = Map.Map Adr HeapValue

data MachineState =
  MachineState
    { instrs    :: Instructions
    , ip        :: IP
    , counter   :: Int
    , heap      :: Heap
    , callStack :: [IP]
    , stack     :: Stack
    }
  deriving (Show)

showMachineState :: MachineState -> String
showMachineState (MachineState instrs ip counter heap callStack stack) =
  "MachineState" ++
  "\n  instrs: " ++
  show (Array.elems instrs) ++
  "\n  ip: " ++
  show ip ++
  "\n  counter: " ++
  show counter ++
  "\n  heap: " ++
  show heap ++
  "\n  callStack: " ++ show callStack ++ "\n  stack: " ++ show stack

showMachineResult :: MachineResult () -> String
showMachineResult mr =
  (case mr of
     Left (ms, Just msg) -> showMachineState ms ++ "\n\nERROR: " ++ msg
     Left (ms, Nothing)  -> showMachineState ms
     Right (ms, ())      -> showMachineState ms) ++
  "\n"

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

halt f = Machine $ \st0 -> Left (st0, Just f)

push :: Int -> Machine ()
push i = Machine $ \st0 -> pure (st0 {stack = i : stack st0}, ())

pushList :: [Int] -> Machine ()
pushList i = Machine $ \st0 -> pure (st0 {stack = i ++ stack st0}, ())

pop :: Machine Int
pop =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:xs) -> Right (st0 {stack = xs}, x)
      _      -> Left (st0, Just "Pop on empty stack")

swap :: Machine ()
swap =
  Machine $ \st0@(MachineState {stack}) ->
    case stack of
      (x:y:xs) -> Right (st0 {stack = y : x : xs}, ())
      _        -> Left (st0, Just "Swap on stack without 2 elements")

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

insertHeap :: Adr -> HeapValue -> Machine ()
insertHeap k v =
  Machine $ \st0@(MachineState {heap}) ->
    Right (st0 {heap = Map.insert k v heap}, ())

lookupHeap :: Adr -> Machine HeapValue
lookupHeap k =
  Machine $ \st0@(MachineState {heap}) ->
    case Map.lookup k heap of
      Nothing ->
        Left
          ( st0
          , Just $
            "Tried to lookup " ++ show k ++ " on the heap but it did not exist")
      Just v -> Right (st0, v)

debugState :: String -> Machine ()
debugState s =
  Machine $ \st0@(MachineState {instrs, ip, stack}) ->
    Right
      ( st0
          { instrs =
              Debug.trace
                ("\nDebug " ++
                 s ++
                 "\ninstr: " ++
                 show (instrs Array.! ip) ++ "\nstack: " ++ show stack)
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

getCallStackFrame :: Machine IP
getCallStackFrame =
  Machine $ \st0@(MachineState {callStack}) -> Right (st0, length callStack)

jmp :: IP -> Machine ()
jmp p =
  Machine $ \st0@(MachineState {instrs, ip}) ->
    if p >= length instrs
      then Left (st0, Just "IP out of bounds")
      else Right (st0 {ip = p}, ())

boolToInt True  = 1
boolToInt False = 0

intToBool 0 = False
intToBool _ = True

boolNot :: Machine ()
boolNot = do
  a <- pop
  push $ boolToInt $ not (intToBool a)

boolOp :: (Bool -> Bool -> Bool) -> Machine ()
boolOp fn = do
  a <- pop
  b <- pop
  push $ boolToInt $ fn (intToBool a) (intToBool b)

comparison :: (Int -> Int -> Bool) -> Machine ()
comparison fn = do
  a <- pop
  b <- pop
  push $ boolToInt $ fn a b

intOp :: (Int -> Int -> Int) -> Machine ()
intOp fn = do
  a <- pop
  b <- pop
  push $ fn a b

jmpIf :: IP -> Machine ()
jmpIf p = do
  a <- pop
  Monad.when (intToBool a) $ jmp p

store :: Adr -> Machine ()
store adr = do
  n <- pop
  d <- Monad.replicateM n pop
  insertHeap adr d

load :: Adr -> Machine ()
load adr = do
  d <- lookupHeap adr
  pushList d

storeDynamic :: Adr -> Machine ()
storeDynamic adr = do
  n <- pop
  d <- Monad.replicateM n pop
  csf <- getCallStackFrame
  insertHeap (adr * csf) d

loadDynamic :: Adr -> Machine ()
loadDynamic adr = do
  csf <- getCallStackFrame
  d <- lookupHeap (adr * csf)
  pushList d

callStatic :: Machine ()
callStatic = do
  pushCallStack
  p <- pop
  jmp p

-- | Expects the function type on top of the stack, then traverses closures until a closure with static instruction pointer is encountered
callDynamic :: Machine ()
callDynamic = do
  fnType <- pop
  case fnType of
    _
      | fnType == tagPrimitiveFnAnd -> boolOp (&&)
    _
      | fnType == tagPrimitiveFnOr -> boolOp (||)
    _
      | fnType == tagPrimitiveFnEquals -> comparison (==)
    _
      | fnType == tagPrimitiveFnLessThan -> comparison (<)
    _
      | fnType == tagPrimitiveFnGreaterThan -> comparison (>)
    _
      | fnType == tagPrimitiveFnAdd -> intOp (+)
    _
      | fnType == tagPrimitiveFnSub -> intOp (-)
    _
      | fnType == tagPrimitiveFnMul -> intOp (*)
    _
      | fnType == tagPrimitiveFnDiv -> intOp div
    _
      | fnType == tagStaticFn -> callStatic
    _
      | fnType == tagDynamicFn -> do
        adr <- pop
        load adr
        callDynamic
    _ ->
      halt $
      "Unrecognized function type tag " ++
      show fnType ++
      ", expected either static (" ++
      show tagStaticFn ++ ") or dynamic (" ++ show tagDynamicFn ++ ")"

ret :: Machine ()
ret = do
  ip <- popCallStack
  jmp ip

counterMax = 1000000000

callStackMax = 100000

nextInstr :: Machine Instruction
nextInstr =
  Machine $ \st0@(MachineState {instrs, ip, counter, callStack}) ->
    case True of
      _
        | length callStack > callStackMax ->
          Left (st0, Just $ "Call stack reached " ++ show callStackMax)
      _
        | counter > counterMax ->
          Left (st0, Just $ "Counter reached " ++ show counterMax)
      _
        | ip >= length instrs -> Left (st0, Just "IP out of bounds")
      _ -> Right (st0 {ip = ip + 1, counter = counter + 1}, instrs Array.! ip)

oneInstruction :: Machine ()
oneInstruction = do
  instr <- nextInstr
  case instr of
    Noop           -> noop
    Exit           -> exit
    Push i         -> push i
    Pop            -> Monad.void pop
    Swap           -> swap
    Dup            -> dup
    Dup2           -> dup2
    Dig2           -> dig2
    Dig3           -> dig3
    Not            -> boolNot
    And            -> boolOp (&&)
    Or             -> boolOp (||)
    LessThan       -> comparison (<)
    GreaterThan    -> comparison (>)
    Equals         -> comparison (==)
    Add            -> intOp (+)
    Sub            -> intOp (-)
    Mul            -> intOp (*)
    Div            -> intOp div
    Jmp p          -> jmp p
    JmpIf p        -> jmpIf p
    Store a        -> store a
    Load a         -> load a
    StoreDynamic a -> storeDynamic a
    LoadDynamic a  -> loadDynamic a
    Call           -> callStatic
    CallDynamic    -> callDynamic
    Ret            -> ret

run :: [Instruction] -> MachineResult ()
run instructions =
  let instrs = Array.listArray (0, length instructions - 1) instructions
   in runMachine loop (MachineState instrs 0 0 Map.empty [] [])
  where
    loop = oneInstruction >> loop

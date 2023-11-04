import           Machine (Instruction (..), MachineResult, run, stack)

-- fib n = fib' n 0 1 0
--   where
--     fib' n i f1 _
--       | n == i = f1
--     fib' n i f1 f0 = fib' n (i + 1) (f1 + f0) f1
fibFunction' offset =
  [ Dup2 -- #begin 0
  , Equals
  , Not
  , JmpIf (9 + offset) -- #loop
  , Pop
  , Pop
  , Swap
  , Pop
  , Ret
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

fibFunction offset =
  let callingCode = [Push 0, Push 1, Push 0, Dig3, Call (6 + offset), Ret]
   in callingCode ++ fibFunction' (length callingCode + offset)

callFibProgram =
  let callingCode = [Push 10, Call 3, Exit]
   in callingCode ++ fibFunction (length callingCode)

-- fibr n = fibr' n
--   where
--     fibr' 0 = 1
--     fibr' 1 = 1
--     fibr' n = fibr' (n - 1) + fibr' (n - 2)
fibRFunction offset =
  [ Dup -- #begin 0
  , Push 2
  , LessThan
  , Not
  , JmpIf (8 + offset) -- #loop
  , Pop
  , Push 1
  , Ret
  , Dup -- #loop
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

callFibRProgram =
  let callingCode = [Push 10, Call 3, Exit]
   in callingCode ++ fibRFunction (length callingCode)

programs = [callFibProgram, callFibRProgram]

showResult :: MachineResult () -> String
showResult mr =
  (case mr of
     Left (ms, Just msg) -> show ms ++ "\n" ++ msg
     Left (ms, Nothing)  -> show $ head $ stack ms
     Right (ms, ())      -> show ms) ++
  "\n"

main = do
  mapM_ (putStrLn . showResult . run) programs

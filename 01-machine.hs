import           Machine (Instruction (..), MachineResult, run)

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
  let callingCode =
        [ Push 0
        , Push 1
        , Push 0
        , Dig3
        , Push (8 + offset)
        , Push 0
        , CallDynamic
        , Ret
        ]
   in callingCode ++ fibFunction' (length callingCode + offset)

callFibProgram =
  let callingCode = [Push 10, Push 5, Push 0, CallDynamic, Exit]
   in callingCode ++ fibFunction (length callingCode)

-- fibr n = fibr' n
--   where
--     fibr' 0 = 1
--     fibr' 1 = 1
--     fibr' n = fibr' (n - 1) + fibr' (n - 2)
fibRFunction offset =
  [ Dup -- #begin 0
  , Push 2
  , GreaterThan
  , Not
  , JmpIf (8 + offset) -- #loop
  , Pop
  , Push 1
  , Ret
  , Dup -- #loop
  , Push 1
  , Swap
  , Sub
  , Push (0 + offset)
  , Push 0
  , CallDynamic
  , Swap
  , Push 2
  , Swap
  , Sub
  , Push (0 + offset)
  , Push 0
  , CallDynamic
  , Add
  , Ret
  ]

callFibRProgram =
  let callingCode = [Push 10, Push 5, Push 0, CallDynamic, Exit]
   in callingCode ++ fibRFunction (length callingCode)

-- [x y] dup2
-- [x y x y] greater
-- [x y b] JmpIf (+1)
-- [x y] Swap
-- [y x] Pop
-- [y] Ret
max3Program =
  let main max3Adr =
        [ Noop
        , Push 56
        , Push 1231
        , Push 5040
        , Push max3Adr
        , Push 0
        , CallDynamic
        , Exit
        ]
      max offset = [Dup2, LessThan, JmpIf (4 + offset), Swap, Pop, Ret]
      max3 maxAdr =
        [ Push maxAdr
        , Push 0
        , CallDynamic
        , Push maxAdr
        , Push 0
        , CallDynamic
        , Ret
        ]
      maxAdr = length (main 0)
      max3Adr = length (main 0) + length (max 0)
      mainA = main max3Adr
      maxA = max maxAdr
      max3A = max3 maxAdr
   in mainA ++ maxA ++ max3A

-- max3 -- Takes the biggest of 3 values
-- max3 :: I>I>I>I
--
-- ((((max3) 5040) 123) 50)
-- Push 50 Push 123 Push 5040 Push max3IP Push 0 Call -- in case of static function (pointer to bytecode and known at assembly time)
-- [50 123 5040 max3IP 0] Call
--   fnType <- pop
-- [50 123 5040 max3IP]
--  pushCallStack
--  p <- pop
-- [50 123 5040]
--  jmp p
-- [result]
--
max3Program2 =
  let main max3Adr =
        [ Noop
        , Push 11
        , Push 100
        , Push 50
        , Push max3Adr
        , Push 0
        , CallDynamic
        , Exit
        ]
      max3 offset =
        [ Dup2
        , LessThan
        , JmpIf (4 + offset)
        , Swap
        , Pop
        , Dup2
        , LessThan
        , JmpIf (9 + offset)
        , Swap
        , Pop
        , Ret
        ]
      max3Adr = length (main 0)
      mainA = main max3Adr
      max3A = max3 $ length (main 0)
   in mainA ++ max3A

-- let max' = max3 5040 in
-- (((max') 123) 50)
-- Push 5040 Push max3IP Push 0 Push 3 Store max'
-- Push 50 Push 123 Push max' Push 1 Call
--
-- [5040 max3IP 0 3] Store max'
-- Push 50 Push 123 Push max' Push 1 Call
-- [50 123 max' 1]
--   fnType <- pop -- callDynamic
-- [50 123 max']
--  adr <- pop
--  d <- readHeap adr
--  pusList d
-- [50 123 5040 max3IP 0]
--  call
--  fnType <- pop
-- [50 123 5040 max3IP]
--  pushCallStack
--  p <- pop
-- [50 123 5040]
--  jmp p
-- [result]
--
closureProgram =
  let closureLabel = 0
      main max3Adr =
        [ Noop
        , Push 5040
        , Push max3Adr
        , Push 0
        , Push 3
        , Store closureLabel
        , Push 50
        , Push 123
        , Push closureLabel
        , Push 1
        , CallDynamic
        , Exit
        ]
      max3 offset =
        [ Dup2
        , LessThan
        , JmpIf (4 + offset)
        , Swap
        , Pop
        , Dup2
        , LessThan
        , JmpIf (9 + offset)
        , Swap
        , Pop
        , Ret
        ]
      max3Adr = length (main 0)
      mainA = main max3Adr
      max3A = max3 $ length (main 0)
   in mainA ++ max3A

-- let max' = max3 5040 in
-- let max'' = max' 123 in
-- (max'' 50)
-- Push 5040 Push max3IP Push 0 Push 3 Store max'
-- Push 123 Load max' Push 4 Store max''
-- Push 50 Load max'' Call
--
-- [5040 max3IP 0 3] Store max'
-- [] Push 123 Load max' Push 4
-- [123 5040 max3IP 0 4] Store max''
-- [] Push 50 Load max''
-- [50 123 5040 max3IP 0] Call
-- [result]
--
closureProgram2 =
  let closureLabel = 0
      closureLabel2 = 1
      main max3Adr =
        [ Noop
        , Push 5040
        , Push max3Adr
        , Push 0
        , Push 3
        , Store closureLabel
        , Push 123
        , Load closureLabel
        , Push 4
        , Store closureLabel2
        , Push 50
        , Load closureLabel2
        , CallDynamic
        , Exit
        ]
      max3 offset =
        [ Dup2
        , LessThan
        , JmpIf (4 + offset)
        , Swap
        , Pop
        , Dup2
        , LessThan
        , JmpIf (9 + offset)
        , Swap
        , Pop
        , Ret
        ]
      max3Adr = length (main 0)
      mainA = main max3Adr
      max3A = max3 $ length (main 0)
   in mainA ++ max3A

programs = [max3Program, max3Program2, closureProgram, closureProgram2]

showResult :: MachineResult () -> String
showResult mr =
  (case mr of
     Left (ms, Just msg) -> show ms ++ "\n" ++ msg
     Left (ms, Nothing)  -> show ms
     Right (ms, ())      -> show ms) ++
  "\n"

main = do
  mapM_ (putStrLn . showResult . run) programs

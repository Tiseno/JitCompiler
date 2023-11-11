import           Machine
import           Test

testProgram :: String -> [Instruction] -> [Int] -> IO ()
testProgram = test (stack . either fst fst . run)

stackManipulation =
  describe
    "stack manipulation"
    [ testProgram "push" [Push 1, Push 3, Exit] [3, 1]
    , testProgram "pop" [Push 0, Pop, Exit] []
    , testProgram "swap" [Push 1, Push 3, Swap, Exit] [1, 3]
    , testProgram "dup" [Push 3, Dup, Exit] [3, 3]
    , testProgram "dup2" [Push 1, Push 3, Dup2, Exit] [3, 1, 3, 1]
    , testProgram "dig" [Push 0, Push 1, Push 2, Push 3, Dig, Exit] [1, 3, 2, 0]
    , testProgram
        "dig3"
        [Push 0, Push 1, Push 2, Push 3, Push 4, Dig3, Exit]
        [1, 4, 3, 2, 0]
    ]

boolean =
  describe
    "boolean"
    [ testProgram "not false" [Push 0, Not, Exit] [1]
    , testProgram "not true" [Push 1, Not, Exit] [0]
    , testProgram "false and false" [Push 0, Push 0, And, Exit] [0]
    , testProgram "false and true" [Push 0, Push 1, And, Exit] [0]
    , testProgram "true and false" [Push 1, Push 0, And, Exit] [0]
    , testProgram "true and true" [Push 1, Push 1, And, Exit] [1]
    , testProgram "false or false" [Push 0, Push 0, Or, Exit] [0]
    , testProgram "false or true" [Push 0, Push 1, Or, Exit] [1]
    , testProgram "true or false" [Push 1, Push 0, Or, Exit] [1]
    , testProgram "true or true" [Push 1, Push 1, Or, Exit] [1]
    ]

comparison =
  describe
    "comparison"
    [ testProgram "2 less than 5" [Push 5, Push 2, LessThan, Exit] [1]
    , testProgram "5 less than 2" [Push 2, Push 5, LessThan, Exit] [0]
    , testProgram "5 less than 5" [Push 5, Push 5, LessThan, Exit] [0]
    , testProgram "2 greater than 5" [Push 5, Push 2, GreaterThan, Exit] [0]
    , testProgram "5 greater than 2" [Push 2, Push 5, GreaterThan, Exit] [1]
    , testProgram "5 greater than 5" [Push 5, Push 5, GreaterThan, Exit] [0]
    , testProgram "2 equals 5" [Push 5, Push 2, Equals, Exit] [0]
    , testProgram "5 equals 2" [Push 2, Push 5, Equals, Exit] [0]
    , testProgram "5 equals 5" [Push 5, Push 5, Equals, Exit] [1]
    ]

arithmetic =
  describe
    "arithmetic"
    [ testProgram "add" [Push 2, Push 3, Add, Exit] [5]
    , testProgram "sub" [Push 2, Push 3, Sub, Exit] [1]
    , testProgram "mul" [Push 2, Push 3, Mul, Exit] [6]
    , testProgram "div" [Push 2, Push 3, Div, Exit] [1]
    ]

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
  , Dig
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
        , Push tagStaticFn
        , CallDynamic
        , Ret
        ]
   in callingCode ++ fibFunction' (length callingCode + offset)

fibLoopProgram =
  let callingCode = [Push 10, Push 5, Push tagStaticFn, CallDynamic, Exit]
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
  , Push tagStaticFn
  , CallDynamic
  , Swap
  , Push 2
  , Swap
  , Sub
  , Push (0 + offset)
  , Push tagStaticFn
  , CallDynamic
  , Add
  , Ret
  ]

fibRProgram =
  let callingCode = [Push 10, Push 5, Push tagStaticFn, CallDynamic, Exit]
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
        , Push tagStaticFn
        , CallDynamic
        , Exit
        ]
      max offset = [Dup2, LessThan, JmpIf (4 + offset), Swap, Pop, Ret]
      max3 maxAdr =
        [ Push maxAdr
        , Push tagStaticFn
        , CallDynamic
        , Push maxAdr
        , Push tagStaticFn
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
-- Push 50 Push 123 Push 5040 Push max3IP Push tagStaticFn Call -- in case of static function (pointer to bytecode and known at assembly time)
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
        , Push 5040
        , Push 50
        , Push max3Adr
        , Push tagStaticFn
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
-- Push 5040 Push max3IP Push tagStaticFn Push 3 Store max'
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
closureMax3Program =
  let closureLabel = 0
      main max3Adr =
        [ Noop
        , Push 5040
        , Push max3Adr
        , Push tagStaticFn
        , Push 3
        , Store closureLabel
        , Push 50
        , Push 123
        , Push closureLabel
        , Push tagDynamicFn
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
-- Push 5040 Push max3IP Push tagStaticFn Push 3 Store max'
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
closureMax3Program2 =
  let closureLabel = 0
      closureLabel2 = 1
      main max3Adr =
        [ Noop
        , Push 5040
        , Push max3Adr
        , Push tagStaticFn
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

programs =
  describe
    "programs"
    [ testProgram "fib loop" fibLoopProgram [89]
    , testProgram "fib rec" fibRProgram [89]
    , testProgram "max3 1" max3Program [5040]
    , testProgram "max3 2" max3Program2 [5040]
    , testProgram "max3 3" closureMax3Program [5040]
    , testProgram "max3 4" closureMax3Program2 [5040]
    ]

main = do
  stackManipulation
  boolean
  comparison
  arithmetic
  programs

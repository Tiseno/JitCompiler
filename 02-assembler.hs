import           Assembler
import           Data.Bifunctor
import           Machine
import           Test

binary = EFn EInt (EFn EInt EInt)

unary = EFn EInt EInt

nestedArithmetic = App (App (Id "*" binary) fiveMinusTwo) nineMinusFour
  where
    fiveMinusTwo = App (App (Id "-" binary) (Const 5)) (Const 2)
    nineMinusFour = App (App (Id "-" binary) (Const 9)) (Const 4)

plusClosure =
  App (Let "x" (App (Id "+" binary) (Const 50)) (Id "x" unary)) (Const 23)

absProgram =
  Let
    "double"
    (Abs ("x", EInt) (App (App (Id "+" binary) (Id "x" EInt)) (Id "x" EInt)))
    (App (Id "double" unary) (Const 44))

fibType = EFn EInt (EFn EInt (EFn EInt (EFn EInt EInt)))

-- fib n = fib' n 0 1 0
--   where
--     fib' n i fn fm =
--       if n == i
--         then fn
--         else fib' n (i + 1) (fn + fm) fn
tailFibProgram =
  Let
    "fibR"
    (Abs
       ("n", EInt)
       (Abs
          ("i", EInt)
          (Abs
             ("fn", EInt)
             (Abs
                ("fm", EInt)
                (If
                   (App (App (Id "==" binary) (Id "n" EInt)) (Id "i" EInt))
                   (Id "fn" EInt)
                   (App
                      (App
                         (App
                            (App (Id "fibR" fibType) (Id "n" EInt))
                            (App (App (Id "+" binary) (Id "i" EInt)) (Const 1)))
                         (App
                            (App (Id "+" binary) (Id "fn" EInt))
                            (Id "fm" EInt)))
                      (Id "fn" EInt)))))))
    (App
       (App (App (App (Id "fibR" fibType) (Const 10)) (Const 0)) (Const 1))
       (Const 0))

-- fibr n = if ((<) n) 2
--   then 1
--   else (((+) (fibr (((-) n) 1))) (fibr (((-) n) 2)))
-- in (fibr 10)
naiveFibProgram =
  Let
    "fibr"
    (Abs
       ("n", EInt)
       (If
          (App (App (Id "<" binary) (Id "n" EInt)) (Const 2))
          (Const 1)
          (App
             (App
                (Id "+" binary)
                (App
                   (Id "fibr" unary)
                   (App (App (Id "-" binary) (Id "n" EInt)) (Const 1))))
             (App
                (Id "fibr" unary)
                (App (App (Id "-" binary) (Id "n" EInt)) (Const 2))))))
    (App (Id "fibr" unary) (Const 10))

testProgram :: String -> Expr -> ([Int], Maybe String) -> IO ()
testProgram =
  test (either (first stack) (bimap stack (const Nothing)) . run . makeProgram)

main =
  describe
    "assembling and running"
    [ testProgram "(5 - 2) * (9 - 4)" nestedArithmetic ([15], Nothing)
    , testProgram "+ closure" plusClosure ([73], Nothing)
    , testProgram "function binding" absProgram ([88], Nothing)
    , testProgram "tail fib" tailFibProgram ([89], Nothing)
    , testProgram "naive fib" naiveFibProgram ([89], Nothing)
    ]

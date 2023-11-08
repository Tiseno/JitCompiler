import           Assembler (EType (..), Expr (..), makeProgram)
import           Machine   (run, showMachineResult)

binary = EFn EInt (EFn EInt EInt)

unary = EFn EInt EInt

fiveMinusTwo = App (App (Id "-" binary) (Const 5)) (Const 2)

nestedArithmetic = App (App (Id "+" binary) fiveMinusTwo) fiveMinusTwo

partialPlus =
  Let
    "x"
    (Const 3737)
    (Let "x" (App (Id "+" binary) (Const 5040)) (App (Id "x" unary) (Const 33)))

nestedLetClosure =
  App (Let "x" (App (Id "+" binary) (Const 5040)) (Id "x" unary)) (Const 2323)

absProgram =
  Let
    "double"
    (Abs ("x", EInt) (App (App (Id "+" binary) (Id "x" EInt)) (Id "x" EInt)))
    (App (Id "double" unary) (Const 44))

-- let rec = \x -> if x == 0 then x else (rec (x - 1))
-- in (rec 10)
recProgram =
  Let
    "rec"
    (Abs
       ("x", EInt)
       (If
          (App (App (Id "==" binary) (Id "x" EInt)) (Const 13))
          (Id "x" EInt)
          (App
             (Id "rec" binary)
             (App (App (Id "-" binary) (Id "x" EInt)) (Const 1)))))
    (App (Id "rec" unary) (Const 15))

-- let fn = \x:I -> ((+) x 3)
-- let f2 = \x:I -> (fn x)
-- in (f2:I->I 3)
nestedLetFnProgram =
  Let
    "fn"
    (Abs ("x", EInt) (App (App (Id "+" binary) (Id "x" EInt)) (Const 3)))
    (Let
       "fm"
       (Abs ("x", EInt) (App (Id "fn" unary) (Id "x" EInt)))
       (App (Id "fm" unary) (Const 3)))

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

main = do
  let (prg, staticLabels) = makeProgram True naiveFibProgram
  putStr "\n"
  print staticLabels
  putStr "\n"
  putStrLn $ showMachineResult $ run prg

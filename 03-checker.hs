import           Assembler
import           Checker
import           Data.Bifunctor
import           Expr
import           Machine
import           Test

unary = CFn CInt CInt

binary = CFn CInt (CFn CInt CInt)

comparison = CFn CInt (CFn CInt CBool)

expressions =
  describe
    "expressions"
    [ test check "const int" (Const 1) (Right (Const 1, CInt))
    , test check "const bool" (ConstBool False) (Right (ConstBool False, CBool))
    , test
        check
        "unbound identifier"
        (Id "x" ())
        (Left "Unbound identifier 'x'")
    , test
        check
        "bound identifier"
        (Let "x" (Const 1) (Id "x" ()))
        (Right (Let "x" (Const 1) (Id "x" CInt), CInt))
    , test
        check
        "bound function"
        (Let
           "fn"
           (Abs ("a", ()) (App (App (Id "+" ()) (Id "a" ())) (Const 5)))
           (Id "fn" ()))
        (Right
           ( Let
               "fn"
               (Abs
                  ("a", CInt)
                  (App (App (Id "+" binary) (Id "a" CInt)) (Const 5)))
               (Id "fn" unary)
           , unary))
    , test check "builtin" (Id "+" ()) (Right (Id "+" binary, binary))
    , test
        check
        "partial"
        (App (Id "+" ()) (Const 1))
        (Right (App (Id "+" binary) (Const 1), unary))
    , test
        check
        "applied"
        (App (App (Id "+" ()) (Const 1)) (Const 2))
        (Right (App (App (Id "+" binary) (Const 1)) (Const 2), CInt))
    , test
        check
        "mismatch 1"
        (App (Id "+" ()) (ConstBool True))
        (Left "Can not unify Int with Bool")
    , test
        check
        "mismatch 2"
        (App (Id "+" ()) (Id "+" ()))
        (Left "Can not unify Int with Int->Int->Int")
    , test
        check
        "if condition"
        (If (Const 1) (Const 1) (Const 1))
        (Left "Expected Bool in if condition but found Int")
    , test
        check
        "if consistent"
        (If (ConstBool True) (ConstBool True) (Const 1))
        (Left
           "Expected both branches of if to be of same type, but they were Bool and Int respectively")
    ]

naiveFibProgram =
  Let
    "fibr"
    (Abs
       ("n", ())
       (If
          (App (App (Id "<" ()) (Id "n" ())) (Const 2))
          (Const 1)
          (App
             (App
                (Id "+" ())
                (App
                   (Id "fibr" ())
                   (App (App (Id "-" ()) (Id "n" ())) (Const 1))))
             (App (Id "fibr" ()) (App (App (Id "-" ()) (Id "n" ())) (Const 2))))))
    (App (Id "fibr" ()) (Const 10))

recursive =
  describe
    "recursive definitions"
    [ test
        check
        "naive fib"
        naiveFibProgram
        (Right
           ( Let
               "fibr"
               (Abs
                  ("n", CInt)
                  (If
                     (App (App (Id "<" comparison) (Id "n" CInt)) (Const 2))
                     (Const 1)
                     (App
                        (App
                           (Id "+" binary)
                           (App
                              (Id "fibr" (CFn CInt CInt))
                              (App (App (Id "-" binary) (Id "n" CInt)) (Const 1))))
                        (App
                           (Id "fibr" (CFn CInt CInt))
                           (App (App (Id "-" binary) (Id "n" CInt)) (Const 2))))))
               (App (Id "fibr" (CFn CInt CInt)) (Const 10))
           , CInt))
    ]

higherOrder =
  describe
    "higher order functions"
    [ test
        check
        "higher order function"
        (Let
           "fn"
           (Abs ("f", ()) (App (App (Id "f" ()) (Const 1)) (Const 5)))
           (Id "fn" ()))
        (Right
           ( Let
               "fn"
               (Abs
                  ("f", binary)
                  (App (App (Id "f" binary) (Const 1)) (Const 5)))
               (Id "fn" CInt)
           , unary))
    , test
        check
        "higher order function"
        (Let
           "fn"
           (Abs ("f", ()) (App (App (Id "f" ()) (Const 1)) (Const 5)))
           (App (Id "fn" ()) (Id "+" ())))
        (Right
           ( Let
               "fn"
               (Abs
                  ("f", binary)
                  (App (App (Id "f" binary) (Const 1)) (Const 5)))
               (Id "fn" CInt)
           , unary))
    ]

doubleProgram =
  Let
    "double"
    (Abs ("x", ()) (App (App (Id "+" ()) (Id "x" ())) (Id "x" ())))
    (App (Id "double" ()) (Const 44))

nestedIfProgram =
  If
    (App
       (App
          (Id "<" ())
          (If (App (App (Id "<" ()) (Const 1)) (Const 2)) (Const 1) (Const 1)))
       (Const 2))
    (If
       (App (App (Id "<" ()) (Const 1)) (Const 2))
       (If (App (App (Id "<" ()) (Const 1)) (Const 2)) (Const 1) (Const 1))
       (Const 1))
    (If (App (App (Id "<" ()) (Const 1)) (Const 2)) (Const 1) (Const 1))

nestedLetProgram =
  Let "a" (Let "a" (Const 1) (Const 1)) (Let "a" (Const 1) (Const 1))

checkAndRun :: Expr () -> Either String ([Int], Maybe String)
checkAndRun e = do
  (cExpr, _) <- check e
  pure $
    either (first stack) (\(ms, ()) -> (stack ms, Nothing)) $
    run $ makeProgramFromChecked cExpr

checkAndRunPrograms =
  describe
    "check and run"
    [ test checkAndRun "double" doubleProgram (Right ([88], Nothing))
    , test checkAndRun "nested if" nestedIfProgram (Right ([1], Nothing))
    , test checkAndRun "nested let" nestedLetProgram (Right ([1], Nothing))
    , test checkAndRun "naive fib" naiveFibProgram (Right ([89], Nothing))
    ]

main = do
  expressions
  recursive
  -- TODO higher order functions are not instantiated correctly
  -- higherOrder
  checkAndRunPrograms

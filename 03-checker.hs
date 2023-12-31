import           Assembler
import           Checker
import           Data.Bifunctor
import           Expr
import           Machine
import           Test

int = CInt

bool = CBool

infixr 1 ~>

(~>) = CFn

intOp = int ~> int ~> int

boolOp = bool ~> bool ~> bool

unary = int ~> int

comparison = int ~> int ~> bool

expressions =
  describe
    "expressions"
    [ test check "const int" (Const 1) (Right (Const 1, CInt))
    , test check "const bool" (ConstBool False) (Right (ConstBool False, CBool))
    , test check "builtin and" (Id "&&" ()) (Right (Id "&&" boolOp, boolOp))
    , test
        check
        "builtin or"
        (App (Id "||" ()) (ConstBool True))
        (Right (App (Id "||" boolOp) (ConstBool True), CFn CBool CBool))
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
                  (App (App (Id "+" intOp) (Id "a" CInt)) (Const 5)))
               (Id "fn" unary)
           , unary))
    , test check "builtin" (Id "+" ()) (Right (Id "+" intOp, intOp))
    , test
        check
        "partial"
        (App (Id "+" ()) (Const 1))
        (Right (App (Id "+" intOp) (Const 1), unary))
    , test
        check
        "applied"
        (App (App (Id "+" ()) (Const 1)) (Const 2))
        (Right (App (App (Id "+" intOp) (Const 1)) (Const 2), CInt))
    , test
        check
        "mismatch 1"
        (App (Id "+" ()) (ConstBool True))
        (Left "Could not unify Int with Bool in application")
    , test
        check
        "mismatch 2"
        (App (Id "+" ()) (Id "+" ()))
        (Left "Could not unify Int with Int->Int->Int in application")
    , test
        check
        "if condition"
        (If (Const 1) (Const 1) (Const 1))
        (Left "Expected Bool in if condition but found Int")
    , test
        check
        "if consistent"
        (If (ConstBool True) (ConstBool True) (Const 1))
        (Left "Could not unify Bool with Int in if expression")
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
                           (Id "+" intOp)
                           (App
                              (Id "fibr" (CFn CInt CInt))
                              (App (App (Id "-" intOp) (Id "n" CInt)) (Const 1))))
                        (App
                           (Id "fibr" (CFn CInt CInt))
                           (App (App (Id "-" intOp) (Id "n" CInt)) (Const 2))))))
               (App (Id "fibr" (CFn CInt CInt)) (Const 10))
           , CInt))
    ]

higherOrder =
  describe
    "higher order functions"
    [ test
        check
        "higher order function 1"
        -- let perform = \f -> (f 2)
        -- in (perform (+ 4))
        (Let
           "perform"
           (Abs ("f", ()) (App (Id "f" ()) (Const 2)))
           (App (Id "perform" ()) (App (Id "+" ()) (Const 4))))
        (Right
           ( Let
               "perform"
               (Abs ("f", int ~> int) (App (Id "f" (int ~> int)) (Const 2)))
               (App
                  (Id "perform" ((int ~> int) ~> int))
                  (App (Id "+" intOp) (Const 4)))
           , CInt))
    , test
        check
        "higher order function 2"
        -- let perform = \f -> (f 2)
        -- in ((perform +) 4)
        (Let
           "perform"
           (Abs ("f", ()) (App (Id "f" ()) (Const 2)))
           (App (App (Id "perform" ()) (Id "+" ())) (Const 4)))
        (Right
           ( Let
               "perform"
               (Abs ("f", intOp) (App (Id "f" intOp) (Const 2)))
               (App
                  (App (Id "perform" (intOp ~> int ~> int)) (Id "+" intOp))
                  (Const 4))
           , CInt))
    , test
        check
        "higher order function 3"
        (Let
           "fn"
           (Abs ("f", ()) (App (App (Id "f" ()) (Const 1)) (Const 5)))
           (App (Id "fn" ()) (Id "+" ())))
        (Right
           ( Let
               "fn"
               (Abs ("f", intOp) (App (App (Id "f" intOp) (Const 1)) (Const 5)))
               (App (Id "fn" (intOp ~> int)) (Id "+" intOp))
           , int))
    , test
        check
        "uninstantiated higher order function"
        (Let
           "fn"
           (Abs ("f", ()) (App (App (Id "f" ()) (Const 1)) (Const 5)))
           (Id "fn" ()))
        (Right
           ( Let
               "fn"
               (Abs
                  ("f", int ~> int ~> CVar "t3")
                  (App
                     (App (Id "f" (int ~> int ~> CVar "t3")) (Const 1))
                     (Const 5)))
               (Id "fn" ((int ~> int ~> CVar "t3") ~> CVar "t3"))
           , (int ~> int ~> CVar "t3") ~> CVar "t3"))
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
  higherOrder
  checkAndRunPrograms

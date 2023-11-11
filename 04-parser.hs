import           Expr
import           Parser
import           Test
import           Tokenizer

main = do
  describe
    "parser"
    [ test
        parseProgram
        "expects expression 1"
        [TClosingParen]
        (Left "Expected expression but found ')'")
    , test
        parseProgram
        "expects expression 2"
        []
        (Left "Expected expression but reached end of input")
    , test parseProgram "identifier" [TId "x"] (Right (Id "x" ()))
    , test
        parseProgram
        "application"
        [TOpenParen, TId "f", TId "a", TClosingParen]
        (Right (App (Id "f" ()) (Id "a" ())))
    , test
        parseProgram
        "let expects identifier"
        [TLet]
        (Left "Expected identifier but reached end of input")
    , test
        parseProgram
        "if expects then"
        [TIf, TBoolLit True, TElse, TIntLit 5, TElse, TIntLit 2]
        (Left "Expected then but found else")
    , test
        parseProgram
        "program"
        [ TLet
        , TId "double"
        , TEqual
        , TLambda "x"
        , TArrow
        , TOpenParen
        , TOpenParen
        , TId "+"
        , TId "x"
        , TClosingParen
        , TId "x"
        , TClosingParen
        , TIn
        , TIf
        , TOpenParen
        , TOpenParen
        , TId "&&"
        , TBoolLit False
        , TClosingParen
        , TBoolLit True
        , TClosingParen
        , TThen
        , TOpenParen
        , TId "double"
        , TIntLit 44
        , TClosingParen
        , TElse
        , TIntLit 5
        ]
        (Right
           (Let
              "double"
              (Abs ("x", ()) (App (App (Id "+" ()) (Id "x" ())) (Id "x" ())))
              (If
                 (App (App (Id "&&" ()) (ConstBool False)) (ConstBool True))
                 (App (Id "double" ()) (Const 44))
                 (Const 5))))
    ]

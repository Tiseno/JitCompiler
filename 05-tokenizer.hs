import           Test
import           Tokenizer

main =
  describe
    "tokenizer"
    [ test tokenize "unrecognized" "." (Left "Unrecognized '.'")
    , test
        tokenize
        "identifer after lambda 1"
        "\\1"
        (Left "Expected identifier after \\ but found '1'")
    , test
        tokenize
        "identifer after lambda 2"
        "\\"
        (Left "Expected identifier after \\ but reached end of input")
    , test
        tokenize
        "identifiers do not start with number"
        "n1 2n"
        (Right [TId "n1", TIntLit 2, TId "n"])
    , test
        tokenize
        "identifiers contain either alphaNum or special"
        "n&m2|+aa>>="
        (Right [TId "n", TId "&", TId "m2", TId "|+", TId "aa", TId ">>="])
    , test
        tokenize
        "handles parenthesis, comment, and keywords"
        "let double = \\x -> -- a comment\n((+ x) x) in if ((&& False) True) then (double 44) else 5"
        (Right
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
           ])
    ]

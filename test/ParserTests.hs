module ParserTests where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import AST
import Parser

parserTests = 
    [ testCase "parseLiteral" literalTest 
    , testCase "parens" parensTest 
    , testCase "binop" binopTest 
    ]

parseTest :: String -> Expression -> Assertion
parseTest actual expected =
    case parseExpression actual of
        Left err -> assertFailure err
        Right parsed -> assertEqual "Wrong parse"
                                    expected parsed

literalTest :: Assertion
literalTest = parseTest "5" (ELit (LInt 5))

parensTest :: Assertion
parensTest = parseTest "(5)" (ELit (LInt 5))
          >> parseTest "(5 )" (ELit (LInt 5))
          >> parseTest "( 5 )" (ELit (LInt 5))
          >> parseTest "( 5)" (ELit (LInt 5))
          
binopTest :: Assertion
binopTest  = parseTest "1+2" (EAdd (ELit (LInt 1)) (ELit (LInt 2)))
          >> parseTest "1 + 2" (EAdd (ELit (LInt 1)) (ELit (LInt 2)))
          >> parseTest "1+ 2" (EAdd (ELit (LInt 1)) (ELit (LInt 2)))
          >> parseTest "1 +2" (EAdd (ELit (LInt 1)) (ELit (LInt 2)))
          >> parseTest "(1 + 2)" (EAdd (ELit (LInt 1)) (ELit (LInt 2)))
          >> parseTest "(1+2)" (EAdd (ELit (LInt 1)) (ELit (LInt 2)))
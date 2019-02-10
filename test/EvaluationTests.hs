module EvaluationTests where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Data.Text (unpack)
import AST
import Evaluation
import Parser

evaluationTests = 
    [ testCase "evalLiteral" literalTest 
    , testCase "binaryPlus" plusTest 
    , testCase "lambdas" lambdasTest 
    ]

evaluateTest :: Expression -> Expression -> Assertion
evaluateTest actual expected = case evaluate actual of
    (Left err, log) -> assertFailure (unpack err)
    (Right e, log) -> assertEqual "Wrong result" e expected


parseAndEvaluateTest :: String -> Expression -> Assertion
parseAndEvaluateTest actual expected =
    case parseExpression actual of
        Left err -> assertFailure err
        Right parsed -> evaluateTest parsed expected

literalTest :: Assertion
literalTest = assertEqual "as" 
                          (ELit (LInt 5)) 
                          (ELit (LInt 5))

plusTest :: Assertion
plusTest = parseAndEvaluateTest "1 + 2" (ELit (LInt 3))

lambdasTest :: Assertion
lambdasTest = parseAndEvaluateTest 
                "(x => x * 1000) (1 + ((x => (y => x + y)) 5 2))"
                (ELit (LInt 8000))
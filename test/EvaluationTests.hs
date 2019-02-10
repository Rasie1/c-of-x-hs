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
    , testCase "matchTest" matchTest
    , testCase "wrongMatch" wrongMatchTest
    , testCase "unapplyPlus" unapplyPlusTest
    ]

evaluateTest :: Expression -> Expression -> Assertion
evaluateTest actual expected = case evaluate actual of
    (Left err, log) -> assertFailure (unpack err)
    (Right e, log) -> assertEqual "Wrong result" e expected

failEvaluationTest :: Expression -> Assertion
failEvaluationTest actual = case evaluate actual of
    (Left err, log) -> mempty
    (Right e, log) -> assertFailure ("Not failed with result: " ++ show e)

parseAndEvaluateTest :: String -> Expression -> Assertion
parseAndEvaluateTest actual expected =
    case parseExpression actual of
        Left err -> assertFailure err
        Right parsed -> evaluateTest parsed expected

parseAndFailEvaluationTest :: String -> Assertion
parseAndFailEvaluationTest actual =
    case parseExpression actual of
        Left err -> assertFailure err
        Right parsed -> failEvaluationTest parsed

literalTest :: Assertion
literalTest = assertEqual "as" 
                          (ELit (LInt 5)) 
                          (ELit (LInt 5))

plusTest :: Assertion
plusTest = parseAndEvaluateTest "1 + 2" (ELit (LInt 3))

-- plusFailTest :: Assertion
-- plusFailTest = parseAndFailEvaluationTest "1 + (+)"

lambdasTest :: Assertion
lambdasTest = parseAndEvaluateTest 
                "(x => x * 1000) (1 + ((x => (y => x + y)) 5 2))"
                (ELit (LInt 8000))

matchTest :: Assertion 
matchTest = parseAndEvaluateTest "(3 => 5) 3" (ELit (LInt 5))

wrongMatchTest :: Assertion 
wrongMatchTest = parseAndFailEvaluationTest "(3 => 5) 2"

unapplyPlusTest :: Assertion 
unapplyPlusTest = parseAndEvaluateTest "(x + 3 => x) 4" (ELit (LInt 1))
               >> parseAndEvaluateTest "(3 + x => x) 4" (ELit (LInt 1))
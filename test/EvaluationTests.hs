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
    , testCase "plusFail" plusFailTest 
    , testCase "simpleLambda" simpleLambda  
    , testCase "simpleLambda2" simpleLambda2 
    , testCase "lambdas" lambdasTest  
    , testCase "matchTest" matchTest
    , testCase "wrongMatch" wrongMatchTest
    , testCase "unapplyPlus" unapplyPlusTest
    , testCase "matchPlus" matchPlusTest
    , testCase "wrongMatchPlus" wrongMatchPlusTest
    , testCase "anyMatchTest" anyMatchTest
    , testCase "thenTest" thenTest
    , testCase "integerTypeTest" integerTypeTest
    , testCase "integerFailTypeTest" integerFailTypeTest
    , testCase "integerLambdaTypeTest" integerTypeTest
    , testCase "integerLambdaFailTypeTest" integerFailTypeTest
    , testCase "equalityTest" equalityTest
    , testCase "equalityFailTest" equalityFailTest
    , testCase "equalityEnvironmentTest" equalityEnvironmentTest
    , testCase "equalityEnvironmentFailTest" equalityEnvironmentFailTest
    , testCase "identityFunction" identityFunction
    , testCase "identityFunctionAndEquality" identityFunctionAndEquality
    , testCase "identityFunctionAndEqualityFail" identityFunctionAndEqualityFail
    , testCase "plusFunction" plusFunction
    , testCase "typedArgumentFunction" typedArgumentFunction
    , testCase "typedArgumentFunctionFail" typedArgumentFunctionFail
    ]

evaluateTest :: Expression -> Expression -> Assertion
evaluateTest actual expected = case evaluate actual of
    (Left err, log) -> assertFailure (unpack err)
    (Right e, log) -> assertEqual "Wrong result" expected e

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

plusFailTest :: Assertion
plusFailTest = parseAndFailEvaluationTest "1 + (+)"

simpleLambda :: Assertion
simpleLambda = parseAndEvaluateTest
                "(x => x + 100) 2"
                (ELit (LInt 102))

simpleLambda2 :: Assertion
simpleLambda2 = parseAndEvaluateTest
                "(x => y => x + y) 2 3"
                (ELit (LInt 5))

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

matchPlusTest :: Assertion
matchPlusTest = parseAndEvaluateTest "(1 + 3 => 1) 4" (ELit (LInt 1))

wrongMatchPlusTest :: Assertion
wrongMatchPlusTest = parseAndFailEvaluationTest "(1 + 5 => 1) 4"

anyMatchTest :: Assertion
anyMatchTest = parseAndEvaluateTest "(3 => 1) _" (ELit (LInt 1))

thenTest :: Assertion
thenTest = parseAndEvaluateTest "3; 4" (ELit (LInt 4))
        >> parseAndEvaluateTest "(x => x * 100) 4; (x => x * 100) 1" (ELit (LInt 100))

integerTypeTest :: Assertion
integerTypeTest = parseAndEvaluateTest "Integer 4" (ELit (LInt 4))

integerFailTypeTest :: Assertion
integerFailTypeTest = parseAndFailEvaluationTest "Boolean 4"

integerLambdaTypeTest :: Assertion
integerLambdaTypeTest = parseAndEvaluateTest "(Integer x => x) 4" (ELit (LInt 4))

integerLambdaFailTypeTest :: Assertion
integerLambdaFailTypeTest = parseAndFailEvaluationTest "(Boolean x => x) 4"

equalityTest :: Assertion
equalityTest = parseAndEvaluateTest "3 = 3" (ELit (LInt 3))

equalityFailTest :: Assertion
equalityFailTest = parseAndFailEvaluationTest "3 = 4"

equalityEnvironmentTest :: Assertion
equalityEnvironmentTest = parseAndEvaluateTest "x = 1; x" (ELit (LInt 3))

equalityEnvironmentFailTest :: Assertion
equalityEnvironmentFailTest = parseAndFailEvaluationTest "x = 3; x = 4"

identityFunction :: Assertion
identityFunction = parseAndEvaluateTest "i x = x; i 5" (ELit (LInt 5))

identityFunctionAndEquality :: Assertion
identityFunctionAndEquality = parseAndEvaluateTest "i x = x; i 5 = 5" (ELit (LInt 5))

identityFunctionAndEqualityFail :: Assertion
identityFunctionAndEqualityFail = parseAndFailEvaluationTest "i x = x; i 5 = 4"

plusFunction :: Assertion
plusFunction = parseAndEvaluateTest "plus x y = x + y; plus 1 2" (ELit (LInt 3))

typedArgumentFunction :: Assertion
typedArgumentFunction = parseAndEvaluateTest "f (Integer x) = x + 1; f 5 = 5" (ELit (LInt 5))

typedArgumentFunctionFail :: Assertion
typedArgumentFunctionFail = parseAndFailEvaluationTest "f (Integer x) = x + 1; f False"
module EvaluationTests where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import AST
import Evaluation

evaluationTests = [testCase "evalLiteral" literalTest]

literalTest :: Assertion
literalTest = assertEqual "as" 
                          (ELit (LInt 5)) 
                          (ELit (LInt 5))
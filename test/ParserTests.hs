module ParserTests where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import AST
import Parser

parserTests = [testCase "parseLiteral" literalTest]

parseTest :: String -> Expression -> Assertion
parseTest actual expected =
    case parseExpression actual of
        Left err -> assertFailure err
        Right parsed -> assertEqual "Wrong parse"
                                    parsed expected

literalTest :: Assertion
literalTest = parseTest "5" (ELit (LInt 5))
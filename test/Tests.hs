import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import ParserTests     (parserTests)
import EvaluationTests (evaluationTests)

main :: IO ()
main = defaultMainWithOpts
       (parserTests ++ evaluationTests)
       mempty
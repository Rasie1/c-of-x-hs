module Main where

import Data.Foldable (for_)
import Parser (parseExpression)
import Evaluation (evaluate)

main :: IO ()
main = do
  inputs <- readFile "code.txt"
  putStrLn "Parsing expression..."
  case parseExpression inputs of
    Left err -> putStrLn err
    Right e -> do putStrLn "Expression:"
                  print e
                  putStrLn "Evaluation log:"
                  case evaluate e of
                    (Left err, log) -> do
                      mapM_ putStrLn log
                      putStrLn $ "exception: " ++ show err
                    (Right e, log) -> do
                      mapM_ putStrLn log
                      putStrLn "Result:"
                      print e

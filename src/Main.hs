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
                  let (result, log) = evaluate e 
                  putStrLn log
                  case result of
                    Left err -> do
                      putStrLn $ "exception: " ++ show err
                    Right e -> do
                      putStrLn "Result:"
                      print e

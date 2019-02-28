module Main where

import Data.Foldable (for_)
import Parser (parseExpression)
import Evaluation (evaluate)


import Data.Text (Text)
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except (Except, ExceptT, runExcept, runExceptT, throwError)

w :: ExceptT Text (WriterT String (State Int)) ()
w = do 
  x <- lift $ get
  tell $ show x ++ "\n"
  lift $ put (x + 1)
  w

testLazyWriter :: IO ()
testLazyWriter = do
  let x = snd . fst $ runState (runWriterT (runExceptT w)) 0
  putStrLn x

main :: IO ()
main = do
  -- testLazyWriter
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

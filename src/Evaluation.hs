{-# language OverloadedStrings #-}
module Evaluation where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Except (Except, ExceptT, runExcept, runExceptT, throwError)
import Control.Monad.Writer.Strict
import Control.Monad.State
import Data.Data
import AST

type Environment = Map Text Expression

type LogMessages = [String]
type IndentationLevel = Int
type EvaluationLogger = State ([String], Int)

increaseDebugIndentation :: EvaluationLogger ()
increaseDebugIndentation = do   
    (xs, level) <- get
    put (xs, level + 1)

decreaseDebugIndentation :: EvaluationLogger ()
decreaseDebugIndentation = do   
    (xs, level) <- get
    put (xs, level -1)

debugLog :: String -> EvaluationLogger ()
debugLog s = do   
    (xs, level) <- get
    let indentation = foldl (++) "" $ replicate level ". "
    let newLogEntry = indentation ++ s
    put (newLogEntry:xs, level)


type Evaluation a = ExceptT Text EvaluationLogger a

makeEnvironment :: Environment
makeEnvironment = Map.empty

getFromEnv :: Environment -> Text -> Evaluation (Maybe Expression)
getFromEnv env var = do
    pure $ Map.lookup var env

unwrap :: Environment -> Expression -> Expression -> Evaluation (Bool, Environment) 
unwrap env binder arg = case binder of 
    EVar var -> pure (True, Map.insert var arg env)
    ELit lit -> do 
        path <- findPath env (toConstr binder) arg
        case path of
            Nothing -> throwError "can't unwrap"
            Just res -> return (res == binder, env)
    EAdd l r -> do
        lVariable <- findPath env (toConstr (EVar "")) l
        rVariable <- findPath env (toConstr (EVar "")) r
        case (lVariable, rVariable) of
            (Just (EVar var), Nothing) -> pure (True, Map.insert var (ESub arg (ELit (LInt 3))) env)
            (Nothing, Just (EVar var)) -> pure (True, Map.insert var (ESub arg (ELit (LInt 3))) env)
            _ -> throwError "can't unwrap"
    _ -> throwError "can't unwrap"
    -- EApp fun arg -> unwrap 

findPath :: Environment -> Constr -> Expression -> Evaluation (Maybe Expression)
findPath env expectedConstr actual = do
    lift $ debugLog "findPath"
    if expectedConstr == toConstr actual
    then do 
        return (Just actual)
    else do
        evaluated <- eval env actual
        if evaluated == actual then return Nothing
                               else findPath env expectedConstr evaluated

calculation env cons op l r = do
    p1 <- findPath env (toConstr (ELit (LInt 0))) l
    p2 <- findPath env (toConstr (ELit (LInt 0))) r
    case (p1, p2) of 
        (Nothing, _) -> throwError "invalid calculation"
        (_, Nothing) -> throwError "invalid calculation"
        (Just (ELit (LInt x)), Just (ELit (LInt y))) -> return (ELit (LInt (op x y)))
        _ -> throwError "invalid calculation operand datatype"


eval :: Environment -> Expression -> Evaluation Expression
eval env e = do
    lift $ debugLog ("Evaluating expression")
    lift $ debugLog (show e)
    lift $ increaseDebugIndentation
    r <- result
    lift $ decreaseDebugIndentation
    lift $ debugLog "Evaluation result"
    lift $ debugLog (show r)
    lift $ debugLog (show (Map.assocs env))
    return r
    where result = case e of
            EVar var -> case Map.lookup var env of
                Nothing -> throwError ("unbound variable " <> showT var)
                Just e -> pure e

            ELit lit -> pure e

            EApp fun arg -> case fun of
                ELam binder body -> 
                    eval (Map.insert binder arg env) body
                EAbs binder body -> do
                    (matched, newEnv) <- unwrap env binder arg
                    if not matched then throwError "can't unwrap"
                                   else eval newEnv body
                EApp fun deeperArg -> do
                    path <- findPath env (toConstr (EAbs ENothing ENothing)) fun
                    case path of
                        Nothing -> invalidApp
                        Just (EAbs binder body) -> do
                            (matched, newEnv) <- unwrap env binder deeperArg
                            if not matched then throwError ("can't unwrap")
                                           else do
                                applied <- eval newEnv (EApp (EAbs binder body) deeperArg)
                                eval newEnv (EApp applied arg)
                _ -> invalidApp
                where invalidApp = throwError ("invalid application of " <> showT fun <> " and " <> showT arg)

            ELam arg fun -> pure e
            
            EAdd  l r -> calculation env EAdd  (+) l r
            EMult l r -> calculation env EMult (*) l r
            ESub  l r -> calculation env ESub  (-) l r
            EAbs binder body -> pure e
            LineBreak e1 e2 -> pure e
            ELet var val e -> eval (Map.insert var val env) e
            EPos pos -> pure e
            ENothing -> pure e

type EvaluationResult = (Either Text Expression, [String])

runEvaluation :: Environment -> Expression -> (Either Text Expression, ([String], Int))
runEvaluation env e = runState (runExceptT (eval env e)) ([], 0)

evaluate :: Expression -> EvaluationResult
evaluate e = let (result, (log, _)) = runEvaluation makeEnvironment e
              in (result, reverse log)

showT :: Show a => a -> Text
showT = Text.pack . show

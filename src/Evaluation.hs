{-# language OverloadedStrings #-}
module Evaluation where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Except (Except, ExceptT, runExcept, runExceptT, throwError)
import Control.Monad.Writer
import Control.Monad.State
import Data.Data
import Data.Maybe
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

unwrap :: Environment -> Expression -> Expression -> Evaluation (Maybe Environment) 
unwrap env binder arg = case binder of 
    EVar var -> pure (Just $ Map.insert var arg env)
    ELit lit -> do 
        path <- findPath env (equalExpression binder) arg
        case path of
            Nothing -> throwError "can't unwrap"
            Just res -> return $ Just env
    EAdd l r -> do
        lVariable <- findPath env (ofConstr (EVar "")) l
        rVariable <- findPath env (ofConstr (EVar "")) r
        case (lVariable, rVariable) of
            (Just (EVar var), Nothing) -> pure (Just $ Map.insert var (ESub arg (ELit (LInt 3))) env)
            (Nothing, Just (EVar var)) -> pure (Just $ Map.insert var (ESub arg (ELit (LInt 3))) env)
            _ -> throwError "can't unwrap"
    _ -> throwError "can't unwrap"
    -- EApp fun arg -> unwrap 

type PathFoundPredicate = Expression -> Bool

ofConstr :: Expression -> PathFoundPredicate
ofConstr constr e = toConstr constr == toConstr e

equalExpression :: Expression -> PathFoundPredicate
equalExpression expected actual = expected == actual

finalResult :: PathFoundPredicate
finalResult e = toConstr (ELit (LInt 0)) == toConstr e

findPath :: Environment -> PathFoundPredicate -> Expression -> Evaluation (Maybe Expression)
findPath env predicate actual = do
    if predicate actual then return (Just actual)
                        else do
        evaluated <- eval env actual
        if evaluated == actual then return Nothing
                            else findPath env predicate evaluated

calculation env cons op l r = do
    p1 <- findPath env (ofConstr (ELit (LInt 0))) l
    p2 <- findPath env (ofConstr (ELit (LInt 0))) r
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
            EApp fun arg -> case fun of
                EAbs binder body -> do
                    newEnv <- unwrap env binder arg
                    case newEnv of 
                        Just env -> eval env body
                        _-> throwError "can't unwrap"
                EApp fun deeperArg -> do
                    path <- findPath env (ofConstr (EAbs ENothing ENothing)) fun
                    case path of
                        Nothing -> invalidApp
                        Just (EAbs binder body) -> do
                            newEnv <- unwrap env binder deeperArg
                            case newEnv of
                                Just env -> do
                                    applied <- eval env (EApp (EAbs binder body) deeperArg)
                                    eval env (EApp applied arg)
                                _ -> throwError ("can't unwrap")
                _ -> invalidApp
                where invalidApp = throwError ("invalid application of " <> showT fun <> " and " <> showT arg)
            EAdd  l r -> calculation env EAdd  (+) l r
            EMult l r -> calculation env EMult (*) l r
            ESub  l r -> calculation env ESub  (-) l r
            ELet var val e -> eval (Map.insert var val env) e
            EThen l r -> do
                newEnv <- unwrap env l EAny
                case newEnv of 
                    Just env -> eval env r
                    _ -> throwError "failed evaluating line"
            _ -> pure e

type EvaluationResult = (Either Text Expression, [String])

runEvaluation :: Environment -> Expression -> (Either Text Expression, ([String], Int))
runEvaluation env e = runState (runExceptT evaluated) ([], 0)
    where evaluated = do path <- findPath env finalResult e
                         case path of
                             Nothing -> throwError ("can't evaluate to printable value")
                             Just e -> return e

evaluate :: Expression -> EvaluationResult
evaluate e = let (result, (log, _)) = runEvaluation makeEnvironment e
              in (result, reverse log)

showT :: Show a => a -> Text
showT = Text.pack . show

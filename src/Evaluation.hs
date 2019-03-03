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

eval :: Environment -> Expression -> Evaluation (Expression, Environment)
eval env e = do
    logger ("Evaluating expression")
    logger ("Env: " ++ show env)
    logger ("Exp: " ++ show e)
    lift $ increaseDebugIndentation
    r <- result
    lift $ decreaseDebugIndentation
    logger "Evaluation result:"
    logger (show r)
    return r
    where result = case e of
            EApp fun arg -> case fun of
                EAbs binder body -> do
                    path <- findPath env ofVar binder
                    let envWithCleanBinder = case path of
                            Just (EVar var) -> Map.delete var env
                            Nothing -> env
                    substituted <- subs envWithCleanBinder binder arg
                    case substituted of 
                        Just (expr, env) -> do 
                            (newVar, newEnv) <- eval env body
                            value <- valueFromVariable env newVar
                            return (value, env)
                        _-> throwError "eval: wrong argument"
                EApp _ _ -> do
                    (evaluated, newEnv) <- eval env fun
                    eval newEnv (EApp evaluated arg)
                EType TInt -> do
                    value <- findPath env (ofConstr (ELit $ LInt 0)) arg 
                    case value of
                        Just (ELit (LInt x)) -> return (ELit (LInt x), env)
                        _ -> throwError "argument is not integer"
                EType TBool -> do
                    value <- findPath env (ofConstr (ELit $ LBool False)) arg 
                    case value of
                        Just (ELit (LBool x)) -> return (ELit (LBool x), env)
                        _ -> throwError "argument is not integer"
                _ -> invalidApp
                where invalidApp = throwError ("invalid application of " <> showT fun <> " and " <> showT arg)
            EAdd  l r -> do
                result <- calculation env EAdd  (+) l r
                return (result, env)
            EMult l r -> do
                result <- calculation env EMult (*) l r
                return (result, env)
            ESub  l r -> do
                result <- calculation env ESub  (-) l r
                return (result, env)
            ELet var val e -> eval (Map.insert var val env) e
            -- EEq l r -> do
            --     newEnv <- subs env l r 
            --     case newEnv of 
            --         Nothing -> pure ENothing
            --         Just newEnv -> pure l
            EThen l r -> do
                substituted <- subs env l EAny
                case substituted of 
                    Just (_, env) -> eval env r
                    _ -> throwError "eval: failed evaluating line"
            ETestRec -> do
                logger "rec"
                eval env e
            _ -> pure (e, env)

subs :: Environment -> Expression -> Expression -> Evaluation (Maybe (Expression, Environment)) 
subs env binder arg = do
    logger ("Substituting expression")
    logger ("Env:    " ++ show env)
    logger ("Binder: " ++ show binder)
    logger ("Arg:    " ++ show arg)
    lift $ increaseDebugIndentation
    r <- result
    lift $ decreaseDebugIndentation
    logger ("Substitution result: ")
    logger (show r)
    -- logger "subs result"
    -- logger (show r)
    -- logger (show (Map.assocs env))
    return r
    where result = case binder of 
                    EVar var -> do
                        logger ("Adding to env as " ++ show var ++ ":")
                        logger (show arg)
                        case Map.lookup var env of
                            Nothing -> pure (Just $ (binder, Map.insert var arg env))
                            Just value -> do 
                                logger ("(not) Intersecting with " ++ show value)
                                subs env value arg
                                -- pure (Just $ Map.insert var arg env)
                    ELit lit -> do 
                        path <- findPath env (equalExpression binder) arg
                        case path of
                            Nothing -> return Nothing -- "subs: can't match literals"
                            Just res -> return $ Just (res, env)
                    EApp fun arg -> do
                        (evaluated, newEnv) <- eval env binder
                        subs env evaluated arg
                    EEq l r -> do 
                        substituted <- subs env l r
                        case substituted of
                            Nothing -> return Nothing
                            Just (expr, env) -> subs env expr arg
                    EAdd l r -> do
                        substitutedL <- subs env l (ESub arg r)
                        substitutedR <- subs env r (ESub arg l)
                        case (substitutedL, substitutedR) of
                            (Just x, Nothing) -> return $ Just x
                            (Nothing, Just x) -> return $ Just x
                            _ -> throwError "subs: ambiguous environments"
                        -- lVariable <- findPath env (ofConstr (EVar "")) l
                        -- rVariable <- findPath env (ofConstr (EVar "")) r
                        -- case (lVariable, rVariable) of
                        --     (Just (EVar var), Nothing) -> pure (Just $ Map.insert var (ESub arg (ELit (LInt 3))) env)
                        --     (Nothing, Just (EVar var)) -> pure (Just $ Map.insert var (ESub arg (ELit (LInt 3))) env)
                        --     _ -> throwError "can't subs"
                    _ -> throwError "subs: no match"
                    -- EApp fun arg -> subs 

type Environment = Map Text Expression

type IndentationLevel = Int
type EvaluationLogger = WriterT String (State IndentationLevel)

type Evaluation a = ExceptT Text EvaluationLogger a
type EvaluationResult = (Either Text Expression, String)

increaseDebugIndentation :: EvaluationLogger ()
increaseDebugIndentation = do   
    level <- lift $ get
    lift $ put (level + 1)

decreaseDebugIndentation :: EvaluationLogger ()
decreaseDebugIndentation = do
    level <- lift $ get
    lift $ put (level - 1)

debugLog :: String -> EvaluationLogger ()
debugLog s = do   
    level <- lift $ get
    let indentation = foldl (++) "" $ replicate level ". "
    let newLogEntry = indentation ++ s ++ "\n"
    tell newLogEntry

logger :: String -> Evaluation ()
logger = lift . debugLog

makeEnvironment :: Environment
makeEnvironment = Map.empty

getFromEnv :: Environment -> Text -> Evaluation (Maybe Expression)
getFromEnv env var = pure $ Map.lookup var env

type PathFoundPredicate = Expression -> Bool

ofConstr :: Expression -> PathFoundPredicate
ofConstr constr e = toConstr constr == toConstr e

ofVar :: PathFoundPredicate
ofVar = ofConstr (EVar "")

ofConstrOrVar :: Expression -> PathFoundPredicate
ofConstrOrVar constr e = ofConstr constr e || ofVar e

equalExpression :: Expression -> PathFoundPredicate
equalExpression expected actual = expected == actual

finalResult :: PathFoundPredicate
finalResult e = toConstr (ELit (LInt 0)) == toConstr e

findPath :: Environment -> PathFoundPredicate -> Expression -> Evaluation (Maybe Expression)
findPath env predicate actual = do
    logger ("Searching for path in " ++ show actual)
    if predicate actual 
        then return $ Just actual 
        else case actual of
            EAny -> return $ Just actual
            EVar var -> case Map.lookup var env of
                Nothing -> return $ Just actual
                Just e -> findPath env predicate e
            _ -> do (evaluated, newEnv) <- eval env actual
                    if evaluated == actual 
                        then return Nothing
                        else findPath env predicate evaluated


-- calculation :: Environment 
--                -> (Expression -> Expression -> Expression) 
--                -> (Int -> Int -> Int) -> Expression -> Expression -> Expression
calculation env cons op l r = do
    p1 <- findPath env (ofConstr (ELit (LInt 0))) l
    p2 <- findPath env (ofConstr (ELit (LInt 0))) r
    logger "Calculation"
    logger ("Env: " ++ show env)
    logger (show p1)
    logger (show p2)
    calculateHelper p1 p2
        where calculateHelper :: Maybe Expression -> Maybe Expression -> Evaluation Expression
              calculateHelper p1 p2 = case (p1, p2) of 
                (Just (ELit (LInt x)), Just (ELit (LInt y))) -> return (ELit (LInt (op x y)))
                (Just (EVar lvar), _) ->
                    case Map.lookup lvar env of
                        Just lval -> calculateHelper (Just lval) p2
                        Nothing -> return $ cons l r
                (_, Just (EVar rvar)) ->
                    case Map.lookup rvar env of
                        Just rval -> calculateHelper p1 (Just rval)
                        Nothing -> return $ cons l r
                (Nothing, _) -> throwError "invalid calculation"
                (_, Nothing) -> throwError "invalid calculation"
                _ -> throwError "invalid calculation operand datatype"

orError :: Text -> Maybe a -> Evaluation a
orError err x = case x of
    Just a -> pure a
    Nothing -> throwError err

valueFromVariable :: Environment -> Expression -> Evaluation Expression
-- getVariable env (EVar x) = case Map.lookup x env of 
--         Just a -> pure a
--         Nothing -> throwError "unbound variable"
valueFromVariable env (EVar x) = orError "unbound variable" (Map.lookup x env)
valueFromVariable env y = pure y

-- typeCheck :: Environment -> Expression -> Expression -> Evaluation Bool
-- typeCheck env t arg = case t of



runEvaluation :: Environment -> Expression -> ((Either Text Expression, String), IndentationLevel)
runEvaluation env e = runState (runWriterT (runExceptT evaluated)) 0
        where evaluated = do
                    path <- findPath env finalResult e
                    case path of
                        Nothing -> throwError "can't evaluate to printable value"
                        Just e -> return e

evaluate :: Expression -> EvaluationResult
evaluate e = fst $ runEvaluation makeEnvironment e

showT :: Show a => a -> Text
showT = Text.pack . show

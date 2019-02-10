{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module AST where

import Prelude hiding (unwords)

import Data.Data
import Data.Typeable
import Data.Text (Text, unwords)
import qualified Data.Text as Text
import Text.Megaparsec (Pos)

-- data Operation
--   = Addition Expression Expression
--   | Abstraction Expression Expression
--   | Application Expression Expression

data Expression
  = EVar Text
  | ELit Lit
  | EApp Expression Expression
  | ELam Text Expression
  | EAdd Expression Expression
  | ESub Expression Expression
  | EMult Expression Expression
  | EAbs Expression Expression
  | LineBreak Expression Expression
  | ELet Text Expression Expression
  | EPos Pos
  | ENothing
  -- | EOperation Operation
  deriving (Eq, Ord, Show, Typeable, Data)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Eq, Ord, Show, Typeable, Data)

data Type
  = TInt
  | TBool
  | TVar Text
  | TFun Type Type
  deriving (Eq, Ord, Show, Typeable, Data)

data Scheme = Scheme [Text] Type






































-- Ignore from here onwards
isFun :: Type -> Bool
isFun ty = case ty of
  TFun _ _ -> True
  _ -> False

prettyType :: Type -> Text
prettyType ty = case ty of
  TVar var -> var
  TInt -> "Int"
  TBool -> "Bool"
  TFun ty1 ty2 ->
    (if isFun ty1 then "(" <> prettyType ty1 <> ")" else prettyType ty1)
    <> " -> " <> prettyType ty2

prettyScheme :: Scheme -> Text
prettyScheme (Scheme [] ty) = prettyType ty
prettyScheme (Scheme vars ty) =
  let
    -- This means we can only print types with a maximum of 26 type
    -- variables (Should be enough for the talk :D)
    vars' = zip vars (map Text.singleton ['a'..'z'])
    renamedTy = foldl renameVar ty vars'
  in
    "forall " <> unwords (map snd vars') <> ". " <> prettyType renamedTy

renameVar :: Type -> (Text, Text) -> Type
renameVar ty (old, new) = case ty of
  TInt -> TInt
  TBool -> TBool
  TVar var -> TVar (if var == old then new else var)
  TFun t1 t2 -> TFun (renameVar t1 (old, new)) (renameVar t2 (old, new))

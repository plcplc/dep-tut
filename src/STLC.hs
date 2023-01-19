{-# LANGUAGE StrictData #-}
-- We want to explore using the dependently typed lambda calculus as an
-- application scripting language.
--
-- Things we want:
-- - To script an application web-frontend without having a complicated heterogeneous build setup.
-- - To dynamically revise application scripts without recompiling the host application.
--
-- Things we might eventually want:
-- - To have an application language with runtimes on multiple architectures
--   that is dynamically extensible but statically typed.
-- - Imagine if you could build a single coherent program where parts executed
-- - on a server, parts in AWS lambda functions, parts on IOS/Android, and parts
-- - in JS/webasm!?
-- - Primitive support for accessing underlying metal/computation substrate (safely?).
-- - Enforcable resource limits
-- - Confidently running untrusted code securely
-- - Principled approach to syntactic sugar. Simple core language with multiple syntax styles.
-- - Principled package ecosystem governance. Controlled fragmentation.
-- - Trivial build system requirements.
-- - Images a la smalltalk and lisp. No files!
-- - Incremental type checking of marginal additions. No waiting to compile the world.
-- - Dynamically/genetically/... optimised JIT compilation, system dependennt.
-- - We should call it "Mala" as in "Mala hotpot": It has all the good stuff!
--
-- # On the web frontend stuff:
--
-- I've been using Elm in the past. While it's a nice language, integrating it
-- in the build system is no small feat.
--
-- Imagine instead that we could just compile a DSL to javascript + html and be
-- done with it.
--
-- # On dynamic revision
-- TODO
--
module DepTutMain where

import Control.Monad

-- * Data types from the Dependently Typed Lambda Calculus tutorial

--
-- (Copied more or less verbatim. Might be tidied up with Trees That Grow.)

data TermInferable
  = TermAnnotated TermCheckable Type
  | TermBound Int
  | TermFree Name
  | TermApplication TermInferable TermCheckable

data TermCheckable
  = TermInferred TermInferable
  | TermLambda TermCheckable

data Type
  = TypeFree Name
  | TypeFunction Type Type
  deriving (Eq, Show)

-- Note: I'm somewhat suspicious that this formulation is counter to our goals
-- (esp. the Global constructor.)
data Name
  = Global String
  | Local Int
  | Quote Int
  deriving (Eq, Show)

data Value
  = ValueLambda (Value -> Value)
  | ValueNeutral Neutral

data Neutral
  = NeutralFree Name
  | NeutralApplication Neutral Value

-- * Evaluation


vfree :: Name -> Value
vfree n = ValueNeutral (NeutralFree n)

evalInferable :: [Value] -> TermInferable -> Value
evalInferable env (TermAnnotated e _) = evalCheckable env e
evalInferable env (TermBound var) = env !! var
evalInferable _ (TermFree name) = vfree name
evalInferable env (TermApplication e1 e2) = case evalInferable env e1 of
  ValueLambda vlam -> vlam (evalCheckable env e2)
  ValueNeutral n -> ValueNeutral (NeutralApplication n (evalCheckable env e2))

evalCheckable :: [Value] -> TermCheckable -> Value
evalCheckable env (TermInferred e) = evalInferable env e
evalCheckable env (TermLambda tlam) = ValueLambda (\v -> evalCheckable (v : env) tlam)

-- * Type Checking

data Kind = Star
  deriving (Show)

data Info = HasKind Kind | HasType Type
  deriving (Show)

type Context = [(Name, Info)]

type Result a = Either String a

throwError :: String -> Either String a
throwError = Left

kindCheck :: Context -> Type -> Kind -> Result ()
kindCheck context (TypeFree x) Star =
  case lookup x context of
    Just (HasKind Star) -> return ()
    Nothing -> throwError "unknown identifier"
    _ -> throwError "kindCheck: impossible?"
kindCheck context (TypeFunction t1 t2) Star = do
  kindCheck context t1 Star
  kindCheck context t2 Star

typeInfer :: Int -> Context -> TermInferable -> Result Type
typeInfer i context (TermAnnotated e t) = do
  kindCheck context t Star
  typeCheck i context e t
  return t
typeInfer _ context (TermFree x) =
  case lookup x context of
    Just (HasType t) -> return t
    Nothing -> throwError "unknown identifier"
    _ -> throwError "typeInfer: impossible? (TermFree)"
typeInfer i context (TermApplication e1 e2) = do
  s <- typeInfer i context e1
  case s of
    TypeFunction t1 t2 -> do
      typeCheck i context e2 t1
      return t2
    _ -> throwError "illegal application"
typeInfer _ _ (TermBound _) = throwError "typeInfer: impossible? (TermBound)"

typeCheck :: Int -> Context -> TermCheckable -> Type -> Result ()
typeCheck i context (TermInferred e) t = do
  t' <- typeInfer i context e
  unless (t == t') (throwError "type mismatch")
typeCheck i context (TermLambda e) (TypeFunction t1 t2) =
  typeCheck (i + 1) ((Local i, HasType t1) : context) (substituteCheckable 0 (TermFree (Local i)) e) t2
typeCheck _ _ _ _ = throwError "type mismatch"

substituteCheckable :: Int -> TermInferable -> TermCheckable -> TermCheckable
substituteCheckable i r (TermInferred e) = TermInferred (substituteInferable i r e)
substituteCheckable i r (TermLambda tlam) = TermLambda (substituteCheckable (i + 1) r tlam)

substituteInferable :: Int -> TermInferable -> TermInferable -> TermInferable
substituteInferable i r (TermAnnotated e t) = TermAnnotated (substituteCheckable i r e) t
substituteInferable i r (TermBound j) = if i == j then r else TermBound j
substituteInferable _ _ (TermFree y) = TermFree y
substituteInferable i r (TermApplication e1 e2) = TermApplication (substituteInferable i r e1) (substituteCheckable i r e2)

-- * Quoting

boundfree :: Int -> Name -> TermInferable
boundfree i (Quote k) = TermBound (i - k - 1)
boundfree _ x = TermFree x

quote :: Int -> Value -> TermCheckable
quote i (ValueLambda f) = TermLambda (quote (i + 1) (f (vfree (Quote i))))
quote i (ValueNeutral n) = TermInferred (neutralQuote i n)

neutralQuote :: Int -> Neutral -> TermInferable
neutralQuote i (NeutralFree x) = boundfree i x
neutralQuote i (NeutralApplication n v) = TermApplication (neutralQuote i n) (quote i v)

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
--   on a server, parts in AWS lambda functions, parts on IOS/Android, and parts
--   in JS/webasm!?
-- - Enforcable resource limits
-- - Confidently running untrusted code securely
-- - Principled approach to syntactic sugar. Simple core language with multiple syntax styles.
-- - Principled package ecosystem governance. Controlled fragmentation.
-- - Primitive support for accessing underlying metal/computation substrate (safely? (*)).
--   - (*): It's not about being able to statically guarrantee 'everything'.
--     Rather, we have escape hatches which we guard carefully through social
--     solutions, i.e. opinioned auditing what gets included in curated
--     packages, culture.
--   - I.e., the JIT compiler should be programmable?
--   - An instance of this could be to have e.g. integers and operations on them
--     defined structurally in the language itself, but, when executed, lowered
--     into actual 'machine' integers, because the the JIT has been fed
--     'knowledge' about integers on the architecture it's executing on and
--     'knowledge' about how to reason about program expressions in relaton to
--     the architecture.
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
-- # Links:
--
-- https://timkellogg.me/blog/2021/01/29/cold-paths
--
--   A cold path is a path through the code or situation that rarely happens. By
--   contrast, hot paths happen frequently. You don’t find bugs in hot paths. By
--   nature, bugs are found in places that you didn’t think to look. Bugs are
--   always in cold paths — every bug is found in a path colder than all the paths
--   you tested.
--   …
--   The practice of avoiding cold paths is often presented as “simple code”.
--   Unfortunately, “simple” has such wildly varying meanings that it’s often
--   antagonistic to use it outside a mathematical setting. I’ve found that
--   centering conversations around “avoiding cold paths” gives more clarity on
--   how to proceed.
--
module DepTutMain where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Trans qualified as E
import Control.Monad.Except qualified as E
import Prelude hiding (pi)
import Language.Haskell.TH.Syntax (Lift)

-- We have a lot of bound but unused names here for clarity.
{-# ANN module ("hlint: ignore Use const" :: String) #-}

{-# ANN module ("hlint: ignore Use id" :: String) #-}

-- * Data types from the Dependently Typed Lambda Calculus tutorial

--
-- (Copied more or less verbatim. Might be tidied up with Trees That Grow.)

data Inferability = Inferable | Checkable

data Term (i :: Inferability) where
  -- TermInferrable
  TermAnnotated :: Term 'Checkable -> Term 'Checkable -> Term 'Inferable
  TermStar :: Term 'Inferable
  TermPi :: Term 'Checkable -> Term 'Checkable -> Term 'Inferable
  TermBound :: Int -> Term 'Inferable
  TermFree :: Name -> Term 'Inferable
  TermApplication :: Term 'Inferable -> Term 'Checkable -> Term 'Inferable
  -- TermCheckable
  TermInferred :: Term 'Inferable -> Term 'Checkable
  TermLambda :: Term 'Checkable -> Term 'Checkable

deriving instance Lift (Term i)

deriving instance Eq (Term i)

deriving instance Show (Term i)

ppTerm :: Term i -> String
ppTerm (TermAnnotated tc1 tc2) = "(" ++ ppTerm tc1 ++ " : " ++ ppTerm tc2 ++ ")"
ppTerm TermStar = "*"
ppTerm (TermPi tc1 tc2) = "∀(" ++ ppTerm tc1 ++ ")." ++ ppTerm tc2
ppTerm (TermBound i) = show i
ppTerm (TermFree n) = show n
ppTerm (TermApplication ti tc) = "(" ++ ppTerm ti ++ " $ " ++ ppTerm tc ++ ")"
ppTerm (TermInferred ti) = ppTerm ti
ppTerm (TermLambda tc) = "λ." ++ ppTerm tc

-- The way I'm thinking of this, 'Global' should only be allowed to be used by
-- the interpreter to recognise its own "exported" symbols. Thus, all programs
-- should have no free variables and all externally given definitions should be
-- lamba bound at the top level.
data Name
  = Global String
  | Local Int
  | Quote Int
  deriving (Eq, Show, Lift)

data Value
  = ValueLambda (Value -> Value)
  | ValueStar
  | ValuePi Value (Value -> Value)
  | ValueNeutral Neutral

data Neutral
  = NeutralFree Name
  | NeutralApplication Neutral Value

-- * Evaluation

vfree :: Name -> Value
vfree n = ValueNeutral (NeutralFree n)

evalInferable :: [Value] -> Term 'Inferable -> Value
evalInferable env (TermAnnotated e _) = evalCheckable env e
evalInferable _ TermStar = ValueStar
evalInferable env (TermPi t1 t2) = ValuePi (evalCheckable env t1) (\x -> evalCheckable (x : env) t2)
evalInferable env (TermBound var) = env !! var
evalInferable _ (TermFree name) = vfree name
evalInferable env (TermApplication e1 e2) = case evalInferable env e1 of
  ValueLambda vlam -> vlam (evalCheckable env e2)
  ValueNeutral n -> ValueNeutral (NeutralApplication n (evalCheckable env e2))
  ValuePi _ _ -> error "impossible: evalInferable ValuePi in Application case"
  ValueStar -> error "impossible: evalInferable ValueStar in Application case"

evalCheckable :: [Value] -> Term 'Checkable -> Value
evalCheckable env (TermInferred e) = evalInferable env e
evalCheckable env (TermLambda tlam) = ValueLambda (\v -> evalCheckable (v : env) tlam)

-- * Type Checking

type Type = Value

type Context = [(Name, Type)]

type Result a = Either String a

-- Circumvent Show's annoying escaping of greek characters for 'instance Show String'.
newtype StringShowRaw = StringShowRaw String

instance Show StringShowRaw where
  show (StringShowRaw s) = s

showContext :: Context -> String
showContext = show . map (second (StringShowRaw . ppTerm . quote 0))

throwError :: String -> Either String a
throwError = Left

typeInfer :: Int -> Context -> Term 'Inferable -> Result Type
typeInfer i context (TermAnnotated e p) = do
  typeCheck i context p ValueStar
  let t = evalCheckable [] p
  typeCheck i context e t
  return t
typeInfer _ _ TermStar = return ValueStar
typeInfer i context (TermPi p p') = do
  typeCheck i context p ValueStar
  let t = evalCheckable [] p
  typeCheck (i + 1) ((Local i, t) : context) (substituteCheckable 0 (TermFree (Local i)) p') ValueStar
  return ValueStar
typeInfer _ context (TermFree x) =
  case lookup x context of
    Just t -> return t
    Nothing -> throwError "unknown identifier"
typeInfer i context (TermApplication e1 e2) = do
  s <- typeInfer i context e1
  case s of
    ValuePi t1 t2 -> do
      typeCheck i context e2 t1
      return (t2 (evalCheckable [] e2))
    x -> throwError $ "illegal application: " ++ ppTerm (quote 0 x)
typeInfer _ _ (TermBound _) = throwError "typeInfer: impossible? (TermBound)"

typeCheck :: Int -> Context -> Term 'Checkable -> Type -> Result ()
typeCheck i context (TermInferred e) t = do
  t' <- typeInfer i context e
  let q = quote 0 t
      q' = quote 0 t'
  unless (q == q') (throwError $ "type mismatch: " ++ showContext context ++ " |- " ++ ppTerm q' ++ " != " ++ ppTerm q)
typeCheck i context (TermLambda e) (ValuePi t1 t2) =
  typeCheck (i + 1) ((Local i, t1) : context) (substituteCheckable 0 (TermFree (Local i)) e) (t2 (vfree (Local i)))
typeCheck _ _ _ _ = throwError "omfg: type mismatch"

substituteCheckable :: Int -> Term 'Inferable -> Term 'Checkable -> Term 'Checkable
substituteCheckable i r (TermInferred e) = TermInferred (substituteInferable i r e)
substituteCheckable i r (TermLambda tlam) = TermLambda (substituteCheckable (i + 1) r tlam)

substituteInferable :: Int -> Term 'Inferable -> Term 'Inferable -> Term 'Inferable
substituteInferable i r (TermAnnotated e t) = TermAnnotated (substituteCheckable i r e) t
substituteInferable _ _ TermStar = TermStar
substituteInferable i r (TermPi t1 t2) = TermPi (substituteCheckable i r t1) (substituteCheckable (i + 1) r t2)
substituteInferable i r (TermBound j) = if i == j then r else TermBound j
substituteInferable _ _ (TermFree y) = TermFree y
substituteInferable i r (TermApplication e1 e2) = TermApplication (substituteInferable i r e1) (substituteCheckable i r e2)

-- * Quoting

boundfree :: Int -> Name -> Term 'Inferable
boundfree i (Quote k) = TermBound (i - k - 1)
boundfree _ x = TermFree x

quote :: Int -> Value -> Term 'Checkable
quote i (ValueLambda f) = TermLambda (quote (i + 1) (f (vfree (Quote i))))
quote _ ValueStar = TermInferred TermStar
quote i (ValuePi v f) = TermInferred (TermPi (quote i v) (quote (i + 1) (f (vfree (Quote i)))))
quote i (ValueNeutral n) = TermInferred (neutralQuote i n)

neutralQuote :: Int -> Neutral -> Term 'Inferable
neutralQuote i (NeutralFree x) = boundfree i x
neutralQuote i (NeutralApplication n v) = TermApplication (neutralQuote i n) (quote i v)

{-

How might a program look?

(Here, we take the syntactical convention that 'α -> β' means '∀_:α.β', i.e.
non-dependent, non polymorphic functions.  Thus, we only use the '∀α:τ.ρ' when
we do a polymorphic or dependent function)

Minimally:

---

main :
  ∀Unit : * →
  unit : Unit →
  ∀IO : (∀_:*.*) →
  return : (∀α:*. IO α) →
  bind : (∀α:*. ∀β:*. IO α → (α -> IO β) -> IO β) →
  printHelloWorld : IO Unit →
  IO Unit

---

Or, more fleshed out:

---

main :

  ∀Unit : * ->
  unit : Unit ->

  ∀IO : (* -> *) ->
  return : (∀α:*. IO α) ->
  bind : (∀α:*.∀β:*. IO α -> (α -> IO β) -> IO β) ->
  eek : (∀α:*. IO α) ->

  ∀List : (* -> *) ->
  cons : (∀α:*. α -> List α -> List α) ->
  nil : (∀α:*. List α) ->
  foldr : ((∀α:*. ∀β:*. (α -> β -> β) -> β -> List α -> β) ->

  ∀Maybe : (* -> *) ->
  nothing : (∀α:*. Maybe α) ->
  just : (∀a:*. α -> Maybe a) ->
  maybe : (∀α:*. ∀β:*. β -> (Maybe α -> β) -> Maybe α -> β) ->

  ∀Nat : * ->
  Zero : Nat ->
  Succ : (Nat -> Nat) ->

  ∀Char : * ->
  ordChar : (Nat -> Char) ->

  ∀JSValue : *
  jsValue : (∀α:*. α -> (Nat -> α) -> (List Char -> α) -> (List JSValue -> α)) -> -- and cases for objects and functions ?
  evalJS : (List Char -> IO JSValue) ->

  IO Unit

---

The idea then is that there's an interpreter implemented in JS that runs in the
browser and which may execute lambdas of this type.

The inclusion above of 'evalJS' is of course unsafe, but via carefully governed
layering we should be able to have something that works on a higher level of
abstraction and requires less than full trust.

-}

-- A small surface DSL, to free us from having to deal with de-bruijn
-- indices when defining test expressions.

class TermSub (i :: Inferability) where
  toCheckable :: Term i -> Term 'Checkable

instance TermSub 'Inferable where
  toCheckable = TermInferred

instance TermSub 'Checkable where
  toCheckable = id

type Exp i = Int -> Term i

runExp :: Exp i -> Term i
runExp = ($ 0)

-- | Dependent function arrows. 'Πx:τ.σ[x]', a.k.a. '{x:τ} -> σ' where 'σ' is
-- indexed by 'x', i.e. 'x' may occur free in 'σ'.
pi :: forall i j. (TermSub i, TermSub j) => Exp i -> (Exp 'Inferable -> Exp j) -> Exp 'Inferable
pi t f i = TermPi (toCheckable $ t i) (toCheckable (f (TermBound . flip (-) (i + 1)) (i + 1)))

-- | Non-dependent function arrows. 'Π_:τ.σ', a.k.a 'τ -> σ'.
(~>) :: forall i j. (TermSub i, TermSub j) => Exp i -> Exp j -> Exp 'Inferable
(~>) a b i = TermPi (toCheckable $ a i) (toCheckable (b (i + 1)))

-- | Non-depenent type quantification, of kind star
forall_ :: forall i. (TermSub i) => (Exp 'Inferable -> Exp i) -> Exp 'Inferable
forall_ = pi star

infixr 4 ~>

star :: Exp 'Inferable
star _ = TermStar

lam :: forall i. TermSub i => (Exp 'Inferable -> Exp i) -> Exp 'Checkable
lam f i = TermLambda (toCheckable $ f (TermBound . flip (-) (i + 1)) (i + 1))

app :: forall i. (TermSub i) => Exp 'Inferable -> Exp i -> Exp 'Inferable
app f x i = TermApplication (f i) (toCheckable $ x i)

ann :: forall i j. (TermSub i, TermSub j) => Exp i -> Exp j -> Exp 'Inferable
ann v t i = TermAnnotated (toCheckable $ v i) (toCheckable $ t 0 {- annotations have separate scope-})

free :: String -> Exp 'Inferable
free n _ = TermFree (Global n)

-- ∀_:*.*
typeTest0 :: Term 'Inferable
typeTest0 = runExp $ pi star (\_ -> star)

-- ∀a:*. a -> a
typeId :: Exp 'Inferable
typeId = pi star (\a -> a ~> a)

valId :: Exp 'Inferable
valId = lam (\_a -> lam (\x -> x)) `ann` typeId

valAppId :: Exp 'Inferable
valAppId = (valId `ann` typeId) `app` free "SomeType" `app` free "foo" -- given context SomeType:*, foo:SomeType

-- ∀Unit:*.∀unit:Unit.∀printHello:Unit.Unit
typeTestMainUnit :: Exp 'Inferable
typeTestMainUnit = pi star (\unitT -> pi unitT (\_unit -> pi unitT (\_printHello -> unitT)))

-- Two possible examples of main : ∀Unit:*. (unit) Unit -> (printHello) Unit -> Unit

valTestMainNoop :: Term 'Inferable
valTestMainNoop = runExp $ lam (\_unitTy -> lam (\_unit -> lam (\_printHello -> _unit))) `ann` typeTestMainUnit

valTestMainPrint :: Term 'Inferable
valTestMainPrint = runExp $ lam (\_unitTy -> lam (\_unit -> lam (\_printHello -> _printHello))) `ann` typeTestMainUnit

-- A version that fails to type check, returning Unit (the type) rather than e.g. unit (the value).

valTestMainFail :: Term 'Inferable
valTestMainFail = runExp $ lam (\_unitTy -> lam (\_unit -> lam (\_printHello -> _unitTy))) `ann` typeTestMainUnit

testMain :: Result Type
testMain = typeInfer 0 [] valTestMainNoop

-- An expresssion executer of expressions of type 'typeTestMainUnit'.
printHelloRunner :: Term 'Inferable -> IO ()
printHelloRunner program = do
  case typeInfer 0 [] program of
    Left e -> putStrLn e
    Right ty ->
      if quote 0 ty == toCheckable (runExp typeTestMainUnit)
        then do
          let linkedProgram = runExp $ const program `app` free "Unit" `app` free "unit" `app` free "printHello"
              evaled = evalInferable [] linkedProgram
          execIO evaled
        else putStrLn ("program not of type " ++ ppTerm (runExp typeTestMainUnit))
  where
    execIO :: Value -> IO ()
    execIO (ValueNeutral (NeutralFree (Global "printHello"))) = putStrLn "Hello"
    execIO (ValueNeutral (NeutralFree (Global "unit"))) = return ()
    execIO v = putStrLn $ "eeek:" ++ ppTerm (quote 0 v)

-- Somewhat more elaborate example, where we have a sequencing operation

-- ∀Unit:*. (unit) Unit -> (sequence) (Unit -> Unit) -> (printHello) Unit -> Unit
typeTestSeq :: Exp 'Inferable
typeTestSeq = pi star (\unitTy -> unitTy {-unit-} ~> (unitTy ~> unitTy ~> unitTy {-sequence-}) ~> unitTy {-printHello-} ~> unitTy)

valTestMainSeq1 :: Term 'Inferable
valTestMainSeq1 = runExp $ lam (\_unitTy -> lam (\_unit -> lam (\sequence -> lam (\printHello -> sequence `app` printHello `app` printHello)))) `ann` typeTestSeq

valTestMainSeq2 :: Term 'Inferable
valTestMainSeq2 = runExp $ lam (\_unitTy -> lam (\unit -> lam (\sequence -> lam (\printHello -> sequence `app` printHello `app` unit)))) `ann` typeTestSeq

-- An expresssion executer of expressions of type 'typeTestMainSeq'.
printHelloSeqRunner :: Term 'Inferable -> IO ()
printHelloSeqRunner program = do
  case typeInfer 0 [] program of
    Left e -> putStrLn e
    Right ty ->
      if quote 0 ty == toCheckable (runExp typeTestSeq)
        then do
          let linkedProgram = runExp $ const program `app` free "Unit" `app` free "unit" `app` free "seq" `app` free "printHello"
              evaled = evalInferable [] linkedProgram
          execIO evaled
        else putStrLn ("program not of type " ++ ppTerm (runExp typeTestMainUnit))
  where
    execIO :: Value -> IO ()
    execIO (ValueNeutral (NeutralApplication (NeutralApplication (NeutralFree (Global "seq")) x) y)) = execIO x >> execIO y
    execIO (ValueNeutral (NeutralFree (Global "printHello"))) = putStrLn "Hello"
    execIO (ValueNeutral (NeutralFree (Global "unit"))) = return ()
    execIO v = putStrLn $ "eeek:" ++ ppTerm (quote 0 v)

-- Even more elaborate example, where we have an IO monad

-- ∀Unit:*. (unit) Unit -> ∀IO:*->*. (return) (∀a:*. a -> IO a) -> (bind) (∀a:*.∀b:*. IO a -> (a -> IO b) -> IO b) -> (printHello) IO Unit -> IO Unit
typeTestIO :: Exp 'Inferable
typeTestIO =
  forall_
    ( \unitTy ->
        unitTy
          ~> pi
            (star ~> star)
            ( \ioTy ->
                returnIO ioTy
                  ~> bindIO ioTy
                  ~> {-printHello-} ioTy `app` unitTy
                  ~> ioTy `app` unitTy
            )
    )
  where
    returnIO :: Exp 'Inferable -> Exp 'Inferable
    returnIO ioTy = forall_ (\a -> a ~> ioTy `app` a)

    bindIO :: Exp 'Inferable -> Exp 'Inferable
    bindIO ioTy = forall_ (\a -> ioTy `app` a ~> forall_ (\b -> (a ~> ioTy `app` b) ~> ioTy `app` b))

valTestIOReturn :: Exp 'Inferable
valTestIOReturn =
  ( lam \unitTy ->
      lam \unit ->
        lam \_ioTy ->
          lam \returnIO ->
            lam \_bindIO ->
              lam \_printHello -> returnIO `app` unitTy `app` unit
  )
    `ann` typeTestIO

valTestIOBind :: Exp 'Inferable
valTestIOBind =
  ( lam \unitTy ->
      lam \unit ->
        lam \_ioTy ->
          lam \returnIO ->
            lam \bindIO ->
              lam \_printHello ->
                bindIO `app` unitTy `app` (returnIO `app` unitTy `app` unit) `app` unitTy `app` (returnIO `app` unitTy)
  )
    `ann` typeTestIO

valTestIOHello2 :: Exp 'Inferable
valTestIOHello2 =
  ( lam \unitTy ->
      lam \_unit ->
        lam \_ioTy ->
          lam \_returnIO ->
            lam \bindIO ->
              lam \printHello ->
                bindIO `app` unitTy `app` printHello `app` unitTy `app` lam \_ -> printHello
  )
    `ann` typeTestIO

newtype ExecIO a = ExecIO {unExecIO :: E.ExceptT String IO a}
  deriving (Functor, Applicative, Monad)

-- Yes, I know 'ExceptT e IO' is considered an anti-pattern. But we make no
-- pretense of ever catching any IO exceptions. For these small experiments we
-- just pretend they don't exist.

runExecIO :: ExecIO a -> IO (Either String a)
runExecIO = E.runExceptT . unExecIO

eek :: String -> ExecIO a
eek e = ExecIO (E.throwError e)

printHello :: ExecIO ()
printHello = ExecIO (E.lift $ putStrLn "Hello")

-- An expresssion executer of expressions of type 'typeTestIO'.
printHelloIORunner :: Term 'Inferable -> IO ()
printHelloIORunner program = do
  case typeInfer 0 [] program of
    Left e -> putStrLn e
    Right ty ->
      if quote 0 ty == toCheckable (runExp typeTestIO)
        then do
          let linkedProgram = runExp $ const program `app` free "Unit" `app` free "unit" `app` free "IO" `app` free "returnIO" `app` free "bindIO" `app` free "printHello"
              evaled = evalInferable [] linkedProgram
          res <- runExecIO $ execIO evaled
          case res of
            Left err -> putStrLn err
            Right (ValueNeutral (NeutralFree (Global "unit"))) -> return ()
            Right v -> putStrLn $ "program did not return (IO Unit)?? : " ++ ppTerm (quote 0 v)

          return ()
        else putStrLn ("program not of type " ++ ppTerm (runExp typeTestIO))
  where
    execIO :: Value -> ExecIO Value
    execIO
      ( ValueNeutral
          ( NeutralApplication
              (NeutralApplication (NeutralFree (Global "returnIO")) _)
              x
            )
        ) =
        return x
    execIO
      ( ValueNeutral
          ( NeutralApplication
              ( NeutralApplication
                  ( NeutralApplication
                      (NeutralApplication (NeutralFree (Global "bindIO")) _a)
                      xValue
                    )
                  _b
                )
              f
            )
        ) = do
        x <- execIO xValue
        case f of
          ValueLambda l -> execIO (l x)
          ValueNeutral n -> execIO (ValueNeutral (NeutralApplication n x))
          v -> eek ("(bindIO): " ++ ppTerm (quote 0 v))
    execIO (ValueNeutral (NeutralFree (Global "printHello"))) = do
      printHello
      return (ValueNeutral (NeutralFree (Global "unit")))
    execIO v = do
      eek $ "wildcard case: " ++ ppTerm (quote 0 v)

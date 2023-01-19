{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DepTutQQ where

import Control.Applicative
import Control.Monad
-- import Text.Parsec
-- import Text.Parsec.Language
-- import Text.Parsec.Token qualified as T

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Functor
import Data.Functor.Identity
import Data.List (elemIndex)
import DepTutMain hiding (Exp)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (Loc (Loc, loc_filename, loc_start), Quasi (qLocation), lift)
import Text.Earley qualified as E
import Prelude hiding (lex)

{-

A small quasi quoter to make life writing deptut terms more liveable:

\* Named variables.
\* Common academic syntax.
  "e : τ"
  "*"
  "Π(x:τ).σ"
  "e1 e2"
  "λx.t"
\* Syntactic shorthands:
  "τ → σ" meaning "Π(_:τ).σ" (Non-dependent function space)
\* TODO: Chuch-style iterative sugar:
  "λ(x:τ1).λ(y:τ2).t {inferable as σ}" meaning "λx.λy.t : τ1 -> τ2 -> σ"
\* TODO: let binding sugar:
  "let x = e1 in e2" meaning "(λx.e2) e1"
  "let {x = e1; y = e2} in e3" meaning "(λx.λy.e3) e1 e2"

examples (declaration with "=" is meta syntax):

id : Πa:*.Π(_:a).a
id : Πa:*.a→a
id = λ_.λx.x

id : Πa:*.a->a (unnecessary)
id = λ(a:*).λ(x:a).x

-}

mala :: QuasiQuoter
mala =
  QuasiQuoter
    { quoteDec = error "no mala for declarations",
      quotePat = error "no mala for patterns",
      quoteType = error "no mala for types",
      quoteExp = \text -> do
        case E.fullParses malaExpParser text of
          ([], report) -> fail (show report)
          (res : _, _) -> lift (res [])

          {-
                  Loc {loc_filename = fileName, loc_start = (startLine, startCol)} <- qLocation
                  let parser = do
                        pos <- getPosition
                        setPosition (pos `setSourceLine` startLine `setSourceColumn` startCol)
                        whiteSpace *> malaExpParser <* eof

                  case runParser parser [] fileName text of
                    Left err -> fail (show err)
                    Right res -> lift res
          -}
    }

type VarStack = [String]

type Exp i = VarStack -> Term i

malaExpParser :: E.Parser String String (Exp 'Checkable)
malaExpParser = E.parser malaExpGrammar

malaExpGrammar :: forall r. E.Grammar r (E.Prod r String Char (Exp 'Checkable))
malaExpGrammar = mdo
  root <- E.rule $ checkable

  -- WTF happens with two-line (type annotated) bind declarations here? b/c `mdo`?

  checkable :: E.Prod r String Char (Exp 'Checkable) <-
    E.rule $
      ((\f v -> TermInferred (f v)) <$> inferable)
        <|> lambda

  inferable :: E.Prod r String Char (Exp 'Inferable) <-
    E.rule $ star <|> variable <|> app

  star :: E.Prod r String Char (VarStack -> Term 'Inferable) <-
    E.rule $ const TermStar <$ symbol "*"

  variable :: E.Prod r String Char (Exp 'Inferable) <-
    E.rule $
      ( \var boundVars ->
          case elemIndex var boundVars of
            Nothing -> error $ var ++ " is not in scope"
            Just i -> TermBound i
      )
        <$> identifier

  app :: E.Prod r String Char (Exp 'Inferable) <-
    E.rule $ (\f x vars -> TermApplication (f vars) (x vars))
      <$> inferable <*> checkable

  lambda :: E.Prod r String Char (Exp 'Checkable) <-
    E.rule $
      (\x body vars -> TermLambda (body (x : vars)))
        <$> (symbol "λ" *> identifier <* symbol ".")
        <*> checkable

  return $ skipSpaces root

symbol :: String -> E.Prod r String Char String
symbol = lexeme . E.list

identifier :: E.Prod r String Char String
identifier = lexeme ((:) <$> E.satisfy isAlpha <*> many (E.satisfy isAlphaNum))

lexeme :: E.Prod r String Char a -> E.Prod r String Char a
lexeme thing = thing <* whitespace

whitespace :: forall r. E.Prod r String Char ()
whitespace = () <$ many (E.satisfy isSpace)

skipSpaces :: E.Prod r String Char a -> E.Prod r String Char a
skipSpaces thing = whitespace *> thing

{-
malaExpParser :: Parsec String VarStack (Term 'Checkable)
malaExpParser = parseCheckable
  where
    parseCheckable :: Parsec String VarStack (Term 'Checkable)
    parseCheckable = parseLambda <|> (TermInferred <$> parseInferable) <|>
       try (parens parseCheckable)

    parseInferable :: Parsec String VarStack (Term 'Inferable)
    parseInferable = parseApp <|> parseInferableNoApp

    parseInferableNoApp :: Parsec String VarStack (Term 'Inferable)
    parseInferableNoApp = parseVar <|> parseStar

    parseLambda :: Parsec String VarStack (Term 'Checkable)
    parseLambda = do
      variable <- symbol "λ" *> identifier <* symbol "."
      modifyState (variable :)
      body <- parseCheckable
      modifyState tail
      return (TermLambda body)

    parseStar :: Parsec String VarStack (Term 'Inferable)
    parseStar = symbol "*" $> TermStar

    parseApp :: Parsec String VarStack (Term 'Inferable)
    parseApp = do
      e1 <- parseInferableNoApp
      e2 <- parseCheckable
      return (TermApplication e1 e2)

    parseVar :: Parsec String VarStack (Term 'Inferable)
    parseVar = do
      variable <- identifier
      boundVariables <- getState
      case elemIndex variable boundVariables of
        Nothing -> fail $ variable ++ " is not in scope"
        Just i -> return $ TermFree (Local i)

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser haskellStyle

lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = T.lexeme lexer

symbol :: String -> ParsecT String u Identity String
symbol = T.symbol lexer

identifier :: ParsecT String u Identity String
identifier = T.identifier lexer

whiteSpace :: ParsecT String u Identity ()
whiteSpace = T.whiteSpace lexer

parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = T.parens lexer

-}

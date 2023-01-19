{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Wiki-like system to make judgements and inferences about statements. To be
-- used as a substrate for politics.
module Main where

import Control.Concurrent.STM as STM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import DepTutMain
import DepTutMainQQ
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
-- import Numeric.Natural
import Text.Read (readMaybe)

type Datum = Text

{-
data Datum
  = Statement Natural Text
  | VoteOnStatement Natural Vote
  deriving (Read, Show)

data Vote = For | Against
  deriving (Read, Show)
-}

data DB = DB
  { db :: [Datum],
    evaluate :: Term 'Inferable -- [Datum] -> Text
  }
  deriving (Show)

main :: IO ()
main = do
  db <-
    newTVarIO
      ( DB
          { db =
              [ "Statement:Free-text: Der skal altid vaskes op efter maden",
                "Vote:0:For"
                -- "Statement Amend-Code:λ..."
              ],
            -- Our first 'evaluate' simply calls votes.
            -- Rather than try to be particularly pure in our approach we
            -- instead rely completely freely on FFI to get things going.


            evaluate = [mala|
                λ(Unit : * ).
                λ(unit : Unit).

                λ(Nat : *).
                λ(z : Nat).
                λ(s : Nat -> Nat).

                λ(List : * → *).
                λ(Nil : Π(a:*). List a).
                λ(Cons : Π(a:*). a -> List a -> List a).

                λ(map : Π(a:*). Π(b:*). (a -> b) -> List a -> List b).
                λ(mapIndex : List Nat -> Π(a:*). List a -> List a).

                λ(Text : * ).
                λ(lines : List Text -> Text).
                λ(concat : List Text -> Text).

                λ(approvedText : Text).
                λ(defeatedText : Text).

                λ(Statement : *).
                λ(Vote : *).
                λ(statementText : Statement -> Text).
                λ(parseStatements : List Text -> List Statement).
                λ(parseVotes : List Text -> List Vote).
                λ(tallyVotes : List Vote -> List Int).

                λ(data : List Text).
                (
                  let
                    statements = parseStatements data
                    votes = parseVotes data
                    approvingVotes : List Nat = tallyApproved votes
                    defeatingVotes : List Nat = tallyDefeated votes
                    approvedStatements = mapIndex approvingVotes Statement statements
                    defeatedStatements = mapIndex defeatingVotes Statement statements
                    CT = Cons Text
                  in lines
                  (
                     concat (CT approved (map Statement Text statementText approvedStatements ))
                            (CT defeated (map Statement Text statementText defeatedStatements ))
                  )
                )
                |]
            {-
             pseudo code:

                let statements : [Text] = parseStatements db
                    votes : [Vote] = parseVotes db
                    approved : [Int] = tallyApproved votes
                    defeated : [Int] = tallyDefeated votes
                    approvedStatements : [Text] = map (index statements) approved
                    defeatedStatements : [Text] = map (index statements) defeated
                 in lines ([ "Approved:" ] ++ approvedStatements ++
                           ["", "Defeated:"] ++ defeatedStatements)

                 where
                   parseStatements :: [Datum] -> [Text]
                   parseVotes :: [Datum] -> [Vote]
                   tallyApproved :: [Vote] -> [Int]
                   tallyDefeated :: [Vote] -> [Int]
                   lines :: [Text] -> Text
                   (++) :: [a] -> [a]


            -}
          }
      )
  run 8181 (ubikApp db)

ubikApp :: TVar DB -> Application
ubikApp db req respond = do
  case (requestMethod req, pathInfo req) of
    (method, ["newdatum"]) | method == methodPost -> do
      d <- (decodeUtf8 . LBS.toStrict) <$> strictRequestBody req
      atomically $ modifyTVar db (\(db'@DB {db = ds}) -> db' {db = (d : ds)})
      respond $ responseLBS status200 [] "OK"
    (method, ["vote"]) | method == methodPost -> do
      d <- (decodeUtf8 . LBS.toStrict) <$> strictRequestBody req
      -- note: This can actually read an arbitrary Datum.
      -- Pretend it only reads votes for now.
      case readMaybe (T.unpack d) of
        Just v -> do
          atomically $ modifyTVar db (\db'@DB {db = ds} -> db' {db = (v : ds)})
          respond $ responseLBS status200 [] "OK"
        Nothing -> respond $ responseLBS status500 [] "Not a vote"
    {-
        (method, ["evaluate"]) | method == methodGet -> do

          DB {db = datums} <- atomically $ readTVar db
          let currentState =
                [ (statement, votesFor, votesAgainst, verdict)
                  | Statement n statement <- datums,
                    let votesFor = length [() | VoteOnStatement m For <- datums, n == m],
                    let votesAgainst = length [() | VoteOnStatement m Against <- datums, n == m],
                    let verdict = case compare votesFor votesAgainst of LT -> Just Against; EQ -> Nothing; GT -> Just For
                ]
          respond $ responseLBS status200 [] (BS.fromStrict $ encodeUtf8 $ tshow currentState)
    -}
    _ -> do
      datums <- atomically $ readTVar db
      respond $ responseLBS status200 [] (BS.fromStrict $ encodeUtf8 $ tshow datums)

tshow :: Show a => a -> Text
tshow = T.pack . show

{-
Commentary from current Philip to past Philip: This is too much up-front
modeling. Rather than trying to come up with a taxonomy of the possible
statements we should start with bare nothing and develop an app around that
where people can do things and extend that as we need. Otherwise it's just
ungrounded fluff.

In fact, we probably can never come up with a static taxonomy, but rather we
should support the continued evolution of the stored data and what can be done
with it.

newtype Term = TermData Text

-- statements may be taken to be true.
-- We just seed our world of statements with some initial static ones, to get
-- the ball rolling.
data Statement
  = IsPerson Term
  | IsURI Term
  | IsDocuments Term
  | IsSummarizedBy Term [Term]
  | IsValidJudgement Judgement
  | IsJudgedValidWhen Statement (Statement -> Ubik Bool)
  | IsUtteredBy Statement Speaker

data Judgement = Judgement
  { variables :: [Text],
    antecedents :: [Statement],
    conclusions :: [Statement]
  }

{-

'God' says: What 'Philip' says is to be taken as true
'Philip' says: The document referenced by "https://arstechnica.com/science/2022/02/renewable-energy-and-land-conservation-dont-clash-as-much-as-feared/" is the text "…"
'Philip' says: The text "…" is comprised of paragraphs ["…1", "…2", ... ]
'Philip' says: The text "…2" is unsubstantiated
'Philip' says: The text "…" is faithfully summarized as "Land area used for renewable energy is unlikely to conflict with sensitive habitats"
'Philip' says: The document referenced by "https://arstechnica.com/science/2022/02/renewable-energy-and-land-conservation-dont-clash-as-much-as-feared/" is truthful

-}

newtype DB = DB {dbStatements :: [(Speaker, Statement)]}

newtype Speaker = Speaker Text

data Ubik a

-- Statements, as uttered by God and admitted to Ubik. Statements by God are taken as true.
god :: Statement -> Ubik ()
god = undefined

-- Statements, as uttered by Philip and admitted to Ubik
philip :: Statement -> Ubik ()
philip = undefined

isStatementTrue :: Statement -> Ubik Bool
isStatementTrue = undefined
-}

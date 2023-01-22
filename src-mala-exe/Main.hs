{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Trans
import Codec.Serialise
import Control.Monad
import Control.Monad.Fix
import Data.Functor
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Zipper
import DepTutMain
import qualified GHC.Generics
import Generics.SOP
import qualified Graphics.Vty as V
import Reflex
import Reflex.Network
import Reflex.Vty
import System.Directory
import qualified Text.Read as Read
import Prelude hiding (pi)

data SerialisedTermCheckable
  = STCTermInferred SerialisedTermInferable
  | STCTermLambda SerialisedTermCheckable
  deriving (GHC.Generics.Generic)

data SerialisedTermInferable
  = STITermStar
  | STITermPi SerialisedTermCheckable SerialisedTermCheckable
  | STITermBound Int
  | STITermFree SerialisedName
  | STITermApplication SerialisedTermInferable SerialisedTermCheckable
  | STITermAnnotated SerialisedTermCheckable SerialisedTermCheckable
  deriving (GHC.Generics.Generic)

data SerialisedName
  = SNGlobal String
  | SNLocal Int
  | SNQuote Int
  deriving (GHC.Generics.Generic)

instance Serialise SerialisedTermCheckable

instance Serialise SerialisedTermInferable

instance Serialise SerialisedName

nameFromDto :: SerialisedName -> Name
nameFromDto (SNGlobal str) = Global str
nameFromDto (SNLocal int) = Local int
nameFromDto (SNQuote int) = Quote int

nameToDto :: Name -> SerialisedName
nameToDto (Global str) = SNGlobal str
nameToDto (Local int) = SNLocal int
nameToDto (Quote int) = SNQuote int

termCheckableFromDto :: SerialisedTermCheckable -> Term 'Checkable
termCheckableFromDto (STCTermInferred tiDto) = TermInferred (termInferableFromDto tiDto)
termCheckableFromDto (STCTermLambda tcDto) = TermLambda (termCheckableFromDto tcDto)

termInferableFromDto :: SerialisedTermInferable -> Term 'Inferable
termInferableFromDto STITermStar = TermStar
termInferableFromDto (STITermPi tcDto1 tcDto2) = TermPi (termCheckableFromDto tcDto1) (termCheckableFromDto tcDto2)
termInferableFromDto (STITermBound i) = TermBound i
termInferableFromDto (STITermFree nameDto) = TermFree (nameFromDto nameDto)
termInferableFromDto (STITermApplication tiDto tcDto) = TermApplication (termInferableFromDto tiDto) (termCheckableFromDto tcDto)
termInferableFromDto (STITermAnnotated tcDto1 tcDto2) = TermAnnotated (termCheckableFromDto tcDto1) (termCheckableFromDto tcDto2)

termCheckableToDto :: Term 'Checkable -> SerialisedTermCheckable
termCheckableToDto (TermInferred ti) = STCTermInferred (termInferableToDto ti)
termCheckableToDto (TermLambda tc) = STCTermLambda (termCheckableToDto tc)

termInferableToDto :: Term 'Inferable -> SerialisedTermInferable
termInferableToDto (TermStar) = STITermStar
termInferableToDto (TermPi tc1 tc2) = STITermPi (termCheckableToDto tc1) (termCheckableToDto tc2)
termInferableToDto (TermBound i) = STITermBound i
termInferableToDto (TermFree name) = STITermFree (nameToDto name)
termInferableToDto (TermApplication ti tc) = STITermApplication (termInferableToDto ti) (termCheckableToDto tc)
termInferableToDto (TermAnnotated tc1 tc2) = STITermAnnotated (termCheckableToDto tc1) (termCheckableToDto tc2)

type Sigh t m =
  ( MonadFix m,
    Adjustable t m,
    HasLayout t m,
    HasFocus t m,
    HasDisplayRegion t m,
    HasImageWriter t m,
    MonadNodeId m,
    MonadHold t m,
    HasInput t m,
    HasFocusReader t m,
    HasTheme t m
  )

-- Generic term editor

newtype Fix f = Fix {unfix :: f (Fix f)}

deriving instance GHC.Generics.Generic (Fix f)

deriving instance Show (f (Fix f)) => Show (Fix f)

data SomeTerm r where
  SomeTermNullary :: SomeTerm r
  SomeTermUnary :: r -> SomeTerm r
  SomeTermBinary :: r -> r -> SomeTerm r
  deriving (GHC.Generics.Generic, Show)

instance Generic (SomeTerm r)

instance HasDatatypeInfo (SomeTerm r)

data TermNodeEditor t term = TermNodeEditor
  { tneReplaceTermEv :: Event t term, -- Fires when the node constructor changes.
    tnePunchHoleEv :: Event t (),
    tneFocusId :: Dynamic t (Maybe FocusId),
    tneValue :: Dynamic t term,
    tneBoundingBox :: Dynamic t Boxes
  }

data BoundingBox = BoundingBox { bbWidth :: Int, bbHeight :: Int }
  deriving (Eq)

instance Show BoundingBox where
  show BoundingBox{..} = show bbWidth <> "x" <> show bbHeight

data Boxes = BoxesHorizontally String [Boxes] | BoxesVertically String [Boxes] | BoxesLeaf String BoundingBox

instance Show Boxes where
  show (BoxesLeaf tag_ b) = show b <> " (" <> tag_ <> ")"
  show b@(BoxesHorizontally tag_ bs) = unlines (("Horizontally(" ++ show (computeBoxes b) ++ "), " <> tag_ <> ":"): concatMap (map ("  " ++) . lines . show) bs)
  show b@(BoxesVertically tag_ bs) = unlines (("Vertically:" ++ show (computeBoxes b) ++ "), " <> tag_ <> ":"): concatMap (map ("  " <>) . lines . show) bs)

computeBoxes :: Boxes -> BoundingBox
computeBoxes (BoxesLeaf _ bb) = bb
computeBoxes (BoxesHorizontally _ bbs) = horizontally $ map computeBoxes bbs
computeBoxes (BoxesVertically _ bbs) = vertically $ map computeBoxes bbs

vertically :: [BoundingBox] -> BoundingBox
vertically = getVertically . mconcat . map Vertically

horizontally :: [BoundingBox] -> BoundingBox
horizontally = getHorizontally . mconcat . map Horizontally

newtype Vertically a = Vertically { getVertically :: a }

instance Semigroup (Vertically BoundingBox) where
  Vertically bb1 <> Vertically bb2 = Vertically BoundingBox { bbWidth = max (bbWidth bb1) (bbWidth bb2),
                             bbHeight = bbHeight bb1 + bbHeight bb2
                             }

instance Monoid (Vertically BoundingBox) where
  mempty = Vertically (BoundingBox 0 0)

newtype Horizontally a = Horizontally { getHorizontally :: a }

instance Semigroup (Horizontally BoundingBox) where
  Horizontally bb1 <> Horizontally bb2 = Horizontally BoundingBox { bbHeight = max (bbHeight bb1) (bbHeight bb2),
                             bbWidth = bbWidth bb1 + bbWidth bb2
                             }

instance Monoid (Horizontally BoundingBox) where
  mempty = Horizontally (BoundingBox 0 0)

instance Reflex t => Functor (TermNodeEditor t) where
  fmap f (TermNodeEditor {..}) = TermNodeEditor {tneReplaceTermEv = fmap f tneReplaceTermEv, tneValue = fmap f tneValue, ..}

switchTermNodeEditor :: (Reflex t) => Dynamic t (TermNodeEditor t term) -> TermNodeEditor t term
switchTermNodeEditor dynTNE =
  TermNodeEditor
    { tneReplaceTermEv = switchDyn (tneReplaceTermEv <$> dynTNE),
      tnePunchHoleEv = switchDyn (tnePunchHoleEv <$> dynTNE),
      tneFocusId = join (tneFocusId <$> dynTNE),
      tneValue = join (tneValue <$> dynTNE),
      tneBoundingBox = join (tneBoundingBox <$> dynTNE)
    }

data (:+:) f g a = InL (f a) | InR (g a)

deriving instance (Show (f a), Show (g a)) => Show ((:+:) f g a)

data Hole f = Hole
  deriving (Show)

bbGrout' :: forall t m a. (Sigh t m ) => Char -> Dynamic t BoundingBox -> m a -> m a
bbGrout' c bbDyn ui = do
  -- o <- askOrientation
  -- grout (fixed (boxEdge <$> o <*> bbDyn)) $ axis (flipOrientation <$> o) flex $ grout (fixed (boxEdge . flipOrientation <$> o <*> bbDyn)) $ axis o flex (fill (pure c) >> ui)
  --
  -- assumes already in 'row' axis.
  grout (fixed $ bbHeight <$> bbDyn) $ row $ grout (fixed $ bbWidth <$> bbDyn) $ {-fill (pure c) >>-} ui

-- Phew. This was more complicated than I thought it would be..
bbGrout :: forall t m a. (Sigh t m ) => Char -> Dynamic t BoundingBox -> m a -> m a
bbGrout c bbDyn ui = do
  -- o <- askOrientation
  -- grout (fixed (boxEdge <$> o <*> bbDyn)) $ axis (flipOrientation <$> o) flex $ grout (fixed (boxEdge . flipOrientation <$> o <*> bbDyn)) $ axis o flex (fill (pure c) >> ui)
  --
  -- assumes already in 'row' axis.
  grout (fixed $ bbWidth <$> bbDyn) $ col $ grout (fixed $ bbHeight <$> bbDyn) $ row $ {-fill (pure c) >>-} ui

{-
  where

    boxEdge :: Orientation -> BoundingBox -> Int
    boxEdge o bb =
      case o of
        Orientation_Row -> bbWidth bb
        Orientation_Column -> bbHeight bb
        -}

flipOrientation :: Orientation -> Orientation
flipOrientation = \case
    Orientation_Row -> Orientation_Column
    Orientation_Column -> Orientation_Row

bbGrout2 :: forall t m a. (Sigh t m ) => Char -> Dynamic t Orientation -> Dynamic t BoundingBox -> m a -> m a
bbGrout2 c currentOrientation bbDyn ui = do
  grout (fixed (boxEdge <$> currentOrientation <*> bbDyn)) $ 
    axis (flipOrientation <$>currentOrientation) flex $ 
      grout (fixed (boxEdge . flipOrientation <$> currentOrientation <*> bbDyn)) $ row $ ({-fill (pure c) >>-} ui)

  where
    boxEdge :: Orientation -> BoundingBox -> Int
    boxEdge o bb =
      case o of
        Orientation_Row -> bbWidth bb
        Orientation_Column -> bbHeight bb

testGrout0 :: (Sigh t m ) => m ()
testGrout0 = do
  row $ grout (fixed 30) $ col $ grout (fixed 1) $ row $ do
    fill (pure '.')
    row $ grout (fixed 23) $ col $ grout (fixed 1) $ row $ do
      fill (pure 'A')
      row $ grout (fixed 1) $ col $ grout (fixed 1) $ row $ do
        fill (pure 'B')
      row $ grout (fixed 22) $ col $ grout (fixed 1) $ row $ do
        fill (pure 'C')
        row $ grout (fixed 6) $ col $ grout (fixed 1) $ row $ do
          fill (pure 'D')
        row $ grout (fixed 16) $ col $ grout (fixed 1) $ row $ do
          fill (pure 'E')

testGrout1 :: (Sigh t m ) => m ()
testGrout1 = do
  row $ grout (fixed 30) $ col $ grout (fixed 1) $ row do
    fill (pure '.')
    grout (fixed 23) $ col $ grout (fixed 1) $ row do
      fill (pure 'A')
      grout (fixed 1) $ col $ grout (fixed 1) $ row do
        fill (pure 'B')
      grout (fixed 22) $ col $ grout (fixed 1) $ row do
        fill (pure 'C')
        grout (fixed 6) $ col $ grout (fixed 1) $ row do
          fill (pure 'D')
        grout (fixed 16) $ col $ grout (fixed 1) $ row do
          fill (pure 'E')

testGrout2 :: (Sigh t m ) => m ()
testGrout2 = do
  row $ grout (fixed 30) $ do
    fill (pure '.')
    grout (fixed 23) $ do
      fill (pure 'A')
      grout (fixed 1) $ do
        fill (pure 'B')
      grout (fixed 22) $ do
        fill (pure 'C')
        grout (fixed 6) $ do
          fill (pure 'D')
        grout (fixed 16) $ do
          fill (pure 'E')

testBbGrout :: (Sigh t m ) => m ()
testBbGrout = do
  bbGrout '.' (pure $ BoundingBox 30 1) $ do
    bbGrout 'A' (pure $ BoundingBox 23 1) $ do
      bbGrout 'B' (pure $ BoundingBox 1 1) blank
      bbGrout 'C' (pure $ BoundingBox 22 1) $ do
        bbGrout 'D' (pure $ BoundingBox 6 1) blank
        bbGrout 'E' (pure $ BoundingBox 16 1) blank

tneGrout :: forall t m a. (Sigh t m) => m (TermNodeEditor t a) -> m (TermNodeEditor t a)
tneGrout tneAction = mdo
  tne <- bbGrout 'T' (computeBoxes <$> tneBoundingBox tne) tneAction
  return tne

termExpressionEditorMain ::
  forall t m term.
  ( Sigh t m, TermExpressionEditor term, Show term) =>
  term -> m (Dynamic t term)
termExpressionEditorMain startTerm = do
  tabNavigation
  localInput censorTab $
    grout flex $
      boxTitle (pure singleBoxStyle) "Generic term editor" $ mdo
        -- row $ mdo
        --   grout flex $ text (T.pack . show <$> (current $ tneValue termDyn))
        row $ grout flex $ mdo
          text (T.pack . show <$> (current $ tneBoundingBox termDyn))

        termDyn <-
          grout flex $
            box
              (pure singleBoxStyle)
              ( mdo
                  tne <- row $ nodeEditor startTerm
                  return tne
              )
        return (tneValue termDyn)
  where
    censorTab :: Event t V.Event -> Event t V.Event
    censorTab = ffilter (/= V.EvKey (V.KChar '\t') [])

class InitialTerm term where
  initialTerm :: term

instance InitialTerm (Fix (Hole :+: term)) where
  initialTerm = Fix (InL Hole)

instance InitialTerm Text where
  initialTerm = ""

class HoleFill term where
  holeFill :: Text -> Maybe term

newtype Generically a = Generically a -- for 'deriving via'.

instance
  (HasDatatypeInfo term, All2 InitialTerm (Code term), Generic term) =>
  HoleFill (Generically term)
  where
  holeFill name = fmap Generically $ makeNode
    where
      dti :: DatatypeInfo (Code term)
      dti = datatypeInfo (Proxy @term)

      makeNode :: Maybe term
      makeNode = to . SOP <$> makeNodeG (constructorInfo dti)

      makeNodeG :: forall xss. (All2 InitialTerm xss) => NP ConstructorInfo xss -> Maybe (NS (NP I) xss)
      makeNodeG Nil = Nothing
      makeNodeG (con :* _) | name == T.pack (constructorName con) = Just (Z $ makeConG con)
      makeNodeG (_ :* cs) | otherwise = S <$> makeNodeG cs

      makeConG :: forall xss. (All InitialTerm xss) => ConstructorInfo xss -> NP I xss
      makeConG _ = hcpure (Proxy @InitialTerm) (I initialTerm)

class NodeEditorSkin term where
  nodeTitle :: term -> Text

instance (HasDatatypeInfo term, Generic term) => NodeEditorSkin (Generically term) where
  nodeTitle (Generically term) = go (constructorInfo $ datatypeInfo (Proxy @term)) (from term)
    where
      go :: forall xss. NP ConstructorInfo xss -> SOP I xss -> Text
      go (info :* _) (SOP (Z _)) = T.pack (constructorName info)
      go (_ :* infos) (SOP (S x)) = go infos (SOP x)
      go Nil (SOP x) = case x of {}

class TermExpressionEditor term where
  nodeEditor :: forall t m. Sigh t m => term -> m (TermNodeEditor t term)

instance TermExpressionEditor Text where
  nodeEditor t = mdo
    tneGrout $ mdo
      (focusId, textRes) <- tile' (fixed widthDyn) 
        (localTheme (fmap (`V.withStyle` V.underline))
          (textInput def {_textInputConfig_initialValue = fromText t}))
      isCursorAtEnd <- holdDyn True ((\TextZipper{..} -> _textZipper_after == "" && null _textZipper_linesAfter ) <$> _textInput_userInput textRes)
      -- isFocusedDyn <- isFocused focusId
      let focusedWidthDyn = (\case {True -> 1; False -> 0}) <$> isCursorAtEnd -- ((&&) <$> isFocusedDyn <*> isCursorAtEnd)
      let widthDyn = focusedWidthDyn + (T.length <$> _textInput_value textRes)
      let bbDyn = (\w -> BoxesLeaf "nodeEditor @Text"$ BoundingBox { bbHeight = 1, bbWidth = w }) <$> widthDyn
      return $ TermNodeEditor never never (pure (Just focusId)) (_textInput_value textRes) bbDyn

instance
  ( HoleFill (term (Fix (Hole :+: term))),
    Generic (term (Fix (Hole :+: term))),
    NodeEditorSkin (term (Fix (Hole :+: term))),
    All2 TermExpressionEditor (Code (term (Fix (Hole :+: term))))
  ) =>
  TermExpressionEditor (Fix (Hole :+: term))
  where
  nodeEditor (Fix alt) = tneGrout $ case alt of
    InL Hole -> updateableEditor holeEditor
    InR term -> updateableEditor $ fmap (fmap (Fix . InR . to)) $ termEditor (nodeTitle term) (from term)
    where
      updateableEditor ::
        forall t m.
        (Sigh t m, HasLayout t m, HasFocus t m) =>
        m (TermNodeEditor t (Fix (Hole :+: term))) ->
        m (TermNodeEditor t (Fix (Hole :+: term)))
      updateableEditor editorAction = mdo
          tneDyn <- networkHold @t @m editorAction updateEditorEv
          let tne = switchTermNodeEditor tneDyn
          let replaceEv = tneReplaceTermEv tne
          let punchHoleEv = tnePunchHoleEv tne
          let focusIdDyn = tneFocusId tne
          let updateEditorEv = leftmost [holeEditor <$ punchHoleEv, nodeEditor <$> replaceEv]
          -- Focus logic is always subtle it seems:
          -- The TNE gets a new focusId only when it is replaced. 
          -- It would be wrong to refocus on 'updateEditorEv', as we don't know
          -- the new focus id the replaced editor gets.
          requestFocus $ fforMaybe (fmap Refocus_Id <$> updated focusIdDyn) id
          return $ tne

      termEditor ::
        forall t m xss.
        ( Sigh t m,
          HasLayout t m,
          HasFocus t m,
          All2 TermExpressionEditor xss
        ) =>
        Text ->
        SOP I xss ->
        m (TermNodeEditor t (SOP I xss))
      termEditor title (SOP (S dataxs)) = fmap (fmap (\(SOP x) -> SOP (S x))) (termEditor title (SOP dataxs))
      termEditor title (SOP (Z args)) = mdo
          (focusId, (punchHoleEv, selectedOrientationDyn)) <- tile' (fixed (pure $ T.length title)) $ mdo
            isFocusedDyn <- focus
            punchHoleEv <- fmap (() <$) (key (V.KBS))
            selectedOrientationDyn <- foldDyn (const flipOrientation) Orientation_Row =<< key (V.KFun 1)
            let nodeStyle =
                  isFocusedDyn <&> \case
                    True -> V.defAttr `V.withStyle` V.reverseVideo
                    False -> V.defAttr
            richText (RichTextConfig (current nodeStyle)) $ pure title
            return (punchHoleEv, selectedOrientationDyn)

          -- I wonder what's a good way to let instances put custom UI between different sub editors.
          -- The use case of this is to mimic conventional syntax, so that instead of presenting:
          --
          --   λ• •
          --
          -- We can do:
          --
          --   λ•.•
          --
          -- (Note the '.')
          --
          -- But is this actually a good idea? are we still just holding
          -- ourselves hostage to the shackles of syntax which this editing
          -- method was supposed to shake?
          --
          -- We also need a way to handle infix operators.
          isFocusedDyn <- isFocused focusId
          let charWhenFocused :: Char -> m ()
              charWhenFocused c = mdo
                grout (fixed 1) $ mdo
                  let nodeStyle =
                        isFocusedDyn <&> \case
                          True -> V.defAttr `V.withForeColor` V.srgbColor 0 0 (0 :: Int)  
                          False -> V.defAttr `V.withForeColor` V.srgbColor 230 230 (230 :: Int)
                  richText (RichTextConfig (current nodeStyle)) $ pure (T.singleton c)

               
          let argEditor :: forall term'. TermExpressionEditor term' => term' -> DynamicWriterT t [Boxes] m (Dynamic t term')
              argEditor term = mdo
                (bbDyn, val) <- lift $  bbGrout2 'a' selectedOrientationDyn (computeBoxes <$> bbDyn) $ mdo 
                  charWhenFocused '('
                  TermNodeEditor{tneBoundingBox, tneValue} <- nodeEditor term
                  charWhenFocused ')'
                  let bbDyn' = do
                           bb <- tneBoundingBox 
                           -- return $ getHorizontally $ mconcat $ map Horizontally [bb, BoundingBox { bbHeight = 1, bbWidth = 2 }]
                           return $ BoxesHorizontally "arg" [bb, BoxesLeaf "\"()\"" $ BoundingBox { bbHeight = 1, bbWidth = 2 }]
                  return (bbDyn', tneValue) -- (tneBoundingBox, tneValue) 
                tellDyn ((:[]) <$> bbDyn)
                return val

          (argValuesDyn, argWidthsDyn) <- axis selectedOrientationDyn flex $ runDynamicWriterT $ 
                        unComp $ hcfor (Proxy @TermExpressionEditor) args (\(I arg) -> Comp (argEditor arg))
          -- let argsBB = (getHorizontally . mconcat . map Horizontally ) <$> argWidthsDyn
          let argsBB = (\case 
                          Orientation_Row -> BoxesHorizontally "all args" 
                          Orientation_Column -> BoxesVertically "all args" 
                        )  <$> selectedOrientationDyn <*> argWidthsDyn

          let bbDyn = do
                bbs <- argsBB
                let titleBB = BoxesLeaf "node title" BoundingBox { bbHeight = 1 , bbWidth = T.length title}
                -- pure $ getHorizontally $ titleBB <> bbs
                pure $ BoxesHorizontally ("node \"" ++ T.unpack title ++ "\"") [ titleBB, bbs]

          return $
            TermNodeEditor
              { tneReplaceTermEv = never,
                tnePunchHoleEv = punchHoleEv,
                tneFocusId = pure (Just focusId),
                tneValue = SOP . Z <$> argValuesDyn,
                tneBoundingBox = bbDyn
              }

      holeEditor :: forall t m. (Sigh t m, HasLayout t m, HasFocus t m) => m (TermNodeEditor t (Fix (Hole :+: term)))
      holeEditor = mdo
          grout
            (fixed 1)
            ( text $
                ( \case
                    False -> "◯"
                    True -> "●"
                )
                  <$> current isFocusedDyn
            )
          (focusId, textRes) <- tile' (fixed textWidthDyn) 
            (localTheme (fmap (`V.withStyle` V.underline)) (textInput def))
          isCursorAtEnd <- holdDyn True ((\TextZipper{..} -> _textZipper_after == "" && null _textZipper_linesAfter ) <$> _textInput_userInput textRes)
          isFocusedDyn <- isFocused focusId
          let focusedWidthDyn = (\case {True -> 1; False -> 0}) <$> isCursorAtEnd -- ((&&) <$> isFocusedDyn <*> isCursorAtEnd)
          let textWidthDyn = focusedWidthDyn + (T.length <$> _textInput_value textRes)
          let replaceEv = fforMaybe (updated $ _textInput_value textRes) holeFill
          let widthDyn = 1 + textWidthDyn

          let bbDyn = (\w -> BoxesLeaf "hole" BoundingBox {bbHeight = 1, bbWidth = w}) <$> widthDyn

          return $ TermNodeEditor (fmap (Fix . InR) replaceEv) never (pure (Just focusId)) (pure $ Fix (InL Hole)) bbDyn

-- * Term editor specialized to Dep-tut

data SurfaceTerm t where
  -- TermInferrable
  STermAnnotated :: t -> t -> SurfaceTerm t
  STermStar :: SurfaceTerm t
  STermPi :: Text -> t -> t -> SurfaceTerm t
  STermBound :: Text -> SurfaceTerm t
  STermApplication :: t -> t -> SurfaceTerm t
  -- TermCheckable
  STermLambda :: Text -> t -> SurfaceTerm t
  deriving (GHC.Generics.Generic, Show)

{-
instance HasDatatypeInfo (SurfaceTerm t)
deriving via
  Generically (SurfaceTerm (Fix (Hole :+: SurfaceTerm)))
  instance
    HoleFill (SurfaceTerm (Fix (Hole :+: SurfaceTerm)))

deriving
  via Generically (SurfaceTerm (Fix (Hole :+: SurfaceTerm)))
  instance NodeEditorSkin (SurfaceTerm (Fix (Hole :+: SurfaceTerm)))
  -}

instance Generic (SurfaceTerm t)


instance HoleFill  (SurfaceTerm (Fix (Hole :+: SurfaceTerm))) where
  holeFill = \case
    ":" -> Just $ STermAnnotated (Fix (InL Hole)) (Fix (InL Hole))
    "*" -> Just $ STermStar
    "π" -> Just $ STermPi "" (Fix (InL Hole)) (Fix (InL Hole))
    "pi" -> Just $ STermPi "" (Fix (InL Hole)) (Fix (InL Hole))
    "%" -> Just $ STermBound ""
    "$" -> Just $ STermApplication(Fix (InL Hole))(Fix (InL Hole))
    "λ" -> Just $ STermLambda "" (Fix (InL Hole))
    "lambda" -> Just $ STermLambda "" (Fix (InL Hole))
    _ -> Nothing

instance NodeEditorSkin (SurfaceTerm t) where
  nodeTitle = \case
    STermAnnotated {} -> ":"
    STermStar {} -> "⛦"
    STermPi {} -> "π"
    STermBound {} -> "%"
    STermApplication {} -> "$"
    STermLambda {} -> "λ"

main :: IO ()
main = do
  putStrLn "~Málà~"
  -- exists <- doesFileExist "mala.cbor"
  -- unless exists $ do
  -- write out an initial program payload for starters.
  mainWidget $ initManager_ $ do
    exitKeyEv <- key V.KEsc

    {-
    grout (fixed 5) $ boxTitle (pure singleBoxStyle) "nested row-col-row" $ row $ grout flex $ mdo
      testGrout0

    grout (fixed 10) $ boxTitle (pure singleBoxStyle) "nested row-col only" $ row $ grout flex $ mdo
      testGrout1

    grout (fixed 5) $ boxTitle (pure singleBoxStyle) "No nested row/col, expected output" $ row $ grout flex $ mdo
      col $ grout (fixed 1) $ testGrout2

    grout (fixed 3) $ boxTitle (pure singleBoxStyle) "dynamic askOrientation" $ row $ grout flex $ mdo
      row $ testBbGrout
    -}

    -- _ <- malaExpressionEditor
    _ <- termExpressionEditorMain (Fix (InL Hole) :: Fix (Hole :+: SurfaceTerm))
    pure $ () <$ exitKeyEv

{-
    writeFileSerialise "mala.cbor" (termInferableToDto (runExp initialProgram))

  program <- termInferableFromDto <$> readFileDeserialise "mala.cbor"
  case typeInfer 0 [] program of
    Left e -> putStrLn e
    Right ty ->
      if quote 0 ty == toCheckable (runExp toplevelTypeSig)
        then do
          let linkedProgram = runExp $ const program `app` free "Unit" `app` free "unit" `app` free "IO" `app` free "returnIO" `app` free "bindIO" `app` free "printHello"
              evaled = evalInferable [] linkedProgram
          res <- runExecIO $ execIO evaled
          case res of
            Left err -> putStrLn err
            Right (ValueNeutral (NeutralFree (Global "unit"))) -> return ()
            Right v -> putStrLn $ "program did not return (IO Unit)?? : " ++ ppTerm (quote 0 v)

          return () else putStrLn ("program not of type " ++ ppTerm (runExp toplevelTypeSig))
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

toplevelTypeSig :: Exp 'Inferable
toplevelTypeSig =
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

initialProgram :: Exp 'Inferable
initialProgram =
  ( lam \unitTy ->
      lam \_unit ->
        lam \_ioTy ->
          lam \_returnIO ->
            lam \bindIO ->
              lam \printHello ->
                bindIO `app` unitTy `app` printHello `app` unitTy `app` lam \_ -> printHello
  )
    `ann` toplevelTypeSig
    -}

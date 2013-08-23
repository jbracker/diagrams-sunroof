{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-|
  The Sunroof backend.
-}
module Diagrams.Backend.Sunroof
  ( SunroofBackend(..), JSRenderOp, Options(..)
  ) where

import Data.Monoid
import Data.Maybe
import Data.Default.Class

import Control.Monad ( when )
import Control.Monad.Reader
import Control.Monad.State

import Diagrams.Prelude as DP
import Diagrams.TwoD.Text ( Text(..) )
import Diagrams.TwoD.Adjust ( adjustDia2D, setDefault2DAttributes )
import Diagrams.TwoD.Path ( FillRuleA, getFillRule )

import Language.Sunroof (JS((:=)), T, JSNumber, (#), js)
import Language.Sunroof as SR
import Language.Sunroof.JS.Canvas (JSCanvas)
import qualified Language.Sunroof.JS.Canvas as SR

-- -----------------------------------------------------------------------
-- Backend Token & State
-- -----------------------------------------------------------------------

data SunroofBackend (t :: T) = SunroofBackend

data CanvasState = CS 
  { currentPos :: (Double, Double) 
  , canvasFillRule :: DP.FillRule
  }

instance Default CanvasState where
  def = CS { currentPos = (0,0), canvasFillRule = DP.Winding }

-- -----------------------------------------------------------------------
-- Backend Render Monad
-- -----------------------------------------------------------------------

type CanvasM t a = StateT CanvasState (ReaderT JSCanvas (JS t)) a

type JSRenderOp t = JSCanvas -> JS t ()

context :: CanvasM t JSCanvas
context = lift ask

r :: (JSRenderOp t) -> CanvasM t ()
r render = context >>= \c -> lift (lift $ c SR.# render)

pos :: CanvasM t (Double, Double)
pos = currentPos `fmap` get

move :: (Double, Double) -> CanvasM t ()
move p = modify (\s -> s { currentPos = p })

-- -----------------------------------------------------------------------
-- Backend Render Operations
-- -----------------------------------------------------------------------

jsP :: (Double, Double) -> (JSNumber, JSNumber)
jsP (x,y) = (js x, js y) 

lineTo' :: (Double, Double) -> CanvasM t ()
lineTo' p' = do
  p <- pos
  let newP = p + p'
  r $ SR.lineTo $ jsP newP
  move newP

moveTo' :: (Double, Double) -> CanvasM t ()
moveTo' p = do
  r $ SR.moveTo $ jsP p
  move p

bezierCurveTo' :: (Double, Double) -> (Double, Double) -> (Double, Double) -> CanvasM t ()
bezierCurveTo' c1 c2 p' = do
  p <- pos
  let newP = p + p'
  r $ SR.bezierCurveTo (jsP $ p + c1) (jsP $ p + c2) (jsP newP)
  move newP

fill' :: CanvasM t ()
fill' = do
  fr <- canvasFillRule `fmap` get
  r $ SR.invoke "fill" $ showFillRuleJS fr

-- -----------------------------------------------------------------------
-- Backend Instances
-- -----------------------------------------------------------------------

instance Backend (SunroofBackend t) R2 where
  data Render  (SunroofBackend t) R2 = SRender (CanvasM t ())
  type Result  (SunroofBackend t) R2 = JSRenderOp t
  data Options (SunroofBackend t) R2 = SunroofOptions 
    { canvasSize :: SizeSpec2D
    }
  
  withStyle :: SunroofBackend t -> Style R2 -> Transformation R2
            -> Render (SunroofBackend t) R2 -> Render (SunroofBackend t) R2
  withStyle _ style trans (SRender render) = SRender $ do
    r $ SR.save -- Open local environment
    setCanvasStyle style -- Style
    render -- Render using the given styles
    r $ setCanvasTrans trans -- Transform
    fill'
    r $ SR.stroke
    r $ SR.restore -- Close local environment

  -- | 'doRender' is used to interpret rendering operations.
  doRender :: SunroofBackend t
           -> Options (SunroofBackend t) R2
           -> Render (SunroofBackend t) R2
           -> Result (SunroofBackend t) R2
  doRender _ _ render = \c -> 
    let sm = unRender render
        rm = evalStateT sm def
    in runReaderT rm c
  
  adjustDia :: Monoid' m 
            => SunroofBackend t -> Options (SunroofBackend t) R2 
            -> QDiagram (SunroofBackend t) R2 m 
            -> (Options (SunroofBackend t) R2, QDiagram (SunroofBackend t) R2 m)
  adjustDia c opts d = adjustDia2D canvasSize setCanvasSize c opts
                       (d DP.# reflectY DP.# defaultStyle) --fcA transparent DP.# lw 0.01)
    where setCanvasSize sz o = o { canvasSize = sz }

instance Monoid (Render (SunroofBackend t) R2) where
  mempty  = SRender $ return ()
  (SRender f1) `mappend` (SRender f2) = SRender $ f1 >> f2

instance Renderable (Segment Closed R2) (SunroofBackend t) where
  render c = render c . (fromSegments :: [Segment Closed R2] -> Path R2) . (:[])

instance Renderable (Trail R2) (SunroofBackend t) where
  render c = render c . pathFromTrail
{-
instance Renderable (Segment Closed R2) (SunroofBackend t) where
  render _ (Linear v) = SRender $ lineTo' (unr2 v)
  render _ (Cubic c1 c2 v) = SRender $ bezierCurveTo' (unr2 c1) (unr2 c2) (unr2 v)

instance Renderable (Trail R2) (SunroofBackend t) where
  render _ (Trail segs c) = SRender $ do
    mapM_ (unRender . render SunroofBackend) segs
    when c $ r $ SR.closePath
-}
instance Renderable (Path R2) (SunroofBackend t) where
  render :: SunroofBackend t -> Path R2 -> Render (SunroofBackend t) (V (Path R2))
  render _ p = SRender $ do
    --mapM_ renderLocTrail $ fmap viewLoc $ pathTrails p
    let fixedTrails = map (\(x:xs) -> (x, xs)) $ filter (not . null) $ fixPath p
    (flip mapM_) (fixPath p) $ \t -> case t of
      [] -> return ()
      (x:xs) -> do
        renderFirstSegment x
        mapM_ renderSegment xs
    --fill'
    --r $ SR.stroke
      where 
        renderFirstSegment :: FixedSegment R2 -> CanvasM t ()
        renderFirstSegment s = do
          case s of
            (FLinear p1 p2) -> r $ SR.moveTo (jsP $ unp2 p1)
            (FCubic p1 c1 c2 p2) -> r $ SR.moveTo (jsP $ unp2 p1)
          renderSegment s
        
        renderSegment :: FixedSegment R2 -> CanvasM t ()
        renderSegment (FLinear p1 p2) = r $ SR.lineTo (jsP $ unp2 p2)
        renderSegment (FCubic p1 c1 c2 p2) = r $ SR.bezierCurveTo (jsP $ unp2 c1) (jsP $ unp2 c2) (jsP $ unp2 p2)
        {-
        renderLocTrail :: (Point R2, Trail R2) -> CanvasM t ()
        renderLocTrail (p, tr) = do
          moveTo' $ unp2 p 
          withTrail renderLineTrail (renderLoopTrail p) tr
        renderLineTrail :: Trail' Line R2 -> CanvasM t ()
        renderLineTrail tr = do
          mapM_ renderSegment $ lineSegments tr
        renderLoopTrail :: Point R2 -> Trail' Loop R2 -> CanvasM t ()
        renderLoopTrail p tr = do
          let (segs, openSeg) = loopSegments tr
          mapM_ renderSegment segs
          renderSegment openSeg
        renderSegment :: Segment c R2 -> CanvasM t ()
        renderSegment (Linear (OffsetClosed p)) = lineTo' (unr2 p)
        renderSegment (Linear OffsetOpen)       = lineTo' (0,0)
        renderSegment (Cubic p1 p2 (OffsetClosed p3)) = bezierCurveTo' (unr2 p1) (unr2 p2) (unr2 p3)
        renderSegment (Cubic p1 p2 OffsetOpen)        = bezierCurveTo' (unr2 p1) (unr2 p2) (0,0)
        -}
instance Renderable Text (SunroofBackend t) where
  render :: SunroofBackend t -> Text -> Render (SunroofBackend t) (V Text)
  render b (Text trans align text) = withStyle b mempty trans $ SRender $ do
      r $ SR.fillText (js text) (0, 0) -- TODO

-- -----------------------------------------------------------------------
-- Utility Functions
-- -----------------------------------------------------------------------

unRender :: (Backend b v, b ~ SunroofBackend t, v ~ R2) => Render b v -> CanvasM t ()
unRender (SRender r) = r

defaultStyle :: Semigroup m => QDiagram b R2 m -> QDiagram b R2 m
defaultStyle d = d DP.# DP.fcA DP.transparent
                   DP.# setDefault2DAttributes

setCanvasTrans :: Transformation R2 -> JSRenderOp t
setCanvasTrans t c = c SR.# SR.transform (js a1) (js a2) (js b1) (js b2) (js c1) (js c2)
  where (a1,a2) = unr2 $ DP.apply t unitX
        (b1,b2) = unr2 $ DP.apply t unitY
        (c1,c2) = unr2 $ transl t

setCanvasStyle :: forall v t. Style v -> CanvasM t ()
setCanvasStyle s = do
  foldr (>>) (return ()) $ catMaybes $ 
    [ handle $ fillColor' . getFillColor
    , handle $ strokeColor' . getLineColor
    , handle $ lineWidth' . getLineWidth
    , handle $ lineJoin' . getLineJoin
    , handle $ lineCap' . getLineCap
    , handle $ globalAlpha' . getOpacity
    , handle $ fillRule'
    ]
  where handle :: (AttributeClass a) => (a -> CanvasM t ()) -> Maybe (CanvasM t ())
        handle f = f `fmap` getAttr s

fillRule' :: FillRuleA -> CanvasM t ()
fillRule' fr = modify (\s -> s { canvasFillRule = getFillRule fr })

strokeColor' :: (Color c) => c -> CanvasM t ()
strokeColor' c = r $ SR.strokeStyle := (js $ showColorJS c)

fillColor' :: (Color c) => c -> CanvasM t ()
fillColor' c = r $ SR.fillStyle := js (showColorJS c)

lineWidth' :: Double -> CanvasM t ()
lineWidth' w = r $ SR.lineWidth := js w

lineCap' :: LineCap -> CanvasM t ()
lineCap' lc = r $ SR.lineCap := (js $ fromLineCap lc)

lineJoin' :: LineJoin -> CanvasM t ()
lineJoin' lj = r $ SR.lineJoin := (js $ fromLineJoin lj)

globalAlpha' :: Double -> CanvasM t ()
globalAlpha' a = r $ SR.globalAlpha := js a

showColorJS :: (Color c) => c -> String
showColorJS c = 
  let s = show . floor . (* 255)
      (r,g,b,a) = colorToSRGBA c
  in concat [ "rgba(", s r, ",", s g, ",", s b, ",", show a, ")" ]

showFillRuleJS :: FillRule -> JSString
showFillRuleJS DP.Winding = SR.string "nonzero"
showFillRuleJS DP.EvenOdd = SR.string "evenodd"

fromLineCap :: LineCap -> String
fromLineCap lc = case lc of
  LineCapRound  -> show "round"
  LineCapSquare -> show "square"
  _             -> show "butt"

fromLineJoin :: LineJoin -> String
fromLineJoin lj = case lj of
  LineJoinRound -> show "round"
  LineJoinBevel -> show "bevel"
  _             -> show "miter"
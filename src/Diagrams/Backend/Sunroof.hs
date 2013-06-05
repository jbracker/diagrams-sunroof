{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
  The Sunroof backend.
-}
module Diagrams.Backend.Sunroof
  ( SunroofBackend(..), JSRenderOp
  ) where

import Data.Monoid
import Data.Maybe
import Data.Default

import Control.Monad ( when )
import Control.Monad.Reader
import Control.Monad.State

import Diagrams.Prelude hiding ( (#) )
import Diagrams.TwoD.Text ( Text(..) )

import Language.Sunroof (JS((:=)), T, JSNumber, (#), js)
import Language.Sunroof.JS.Canvas (JSCanvas)
import qualified Language.Sunroof.JS.Canvas as SR

-- -----------------------------------------------------------------------
-- Backend Token & State
-- -----------------------------------------------------------------------

data SunroofBackend (t :: T) = SunroofBackend

data CanvasState = CS { currentPos :: (Double, Double) }

instance Default CanvasState where
  def = CS { currentPos = (0,0) }

-- -----------------------------------------------------------------------
-- Backend Render Monad
-- -----------------------------------------------------------------------

type CanvasM t a = StateT CanvasState (ReaderT JSCanvas (JS t)) a

type JSRenderOp t = JSCanvas -> JS t ()

context :: CanvasM t JSCanvas
context = lift ask

r :: (JSRenderOp t) -> CanvasM t ()
r render = context >>= \c -> lift (lift $ c # render)

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

-- -----------------------------------------------------------------------
-- Backend Instances
-- -----------------------------------------------------------------------

instance Backend (SunroofBackend t) R2 where
  data Render  (SunroofBackend t) R2 = SRender (CanvasM t ())
  type Result  (SunroofBackend t) R2 = JSRenderOp t
  data Options (SunroofBackend t) R2 = NoOptions
  
  withStyle :: SunroofBackend t -> Style R2 -> Transformation R2
            -> Render (SunroofBackend t) R2 -> Render (SunroofBackend t) R2
  withStyle _ style trans (SRender render) = SRender $ do
    r $ SR.save -- Open local environment
    -- Apply styles
    setCanvasStyle style
    r $ setCanvasTrans trans
    render -- Render using the given styles
    r $ SR.restore -- Close local environment

  -- | 'doRender' is used to interpret rendering operations.
  doRender :: SunroofBackend t          -- ^ Backend token (needed only for type inference)
           -> Options (SunroofBackend t) R2  -- ^ Backend-specific collection of rendering options
           -> Render (SunroofBackend t) R2   -- ^ Rendering operation to perform
           -> Result (SunroofBackend t) R2   -- ^ Output of the rendering operation
  doRender _ _ render = \c -> 
    let sm = unRender render
        rm = evalStateT sm def
    in runReaderT rm c

instance Monoid (Render (SunroofBackend t) R2) where
  mempty  = SRender $ return ()
  (SRender f1) `mappend` (SRender f2) = SRender $ f1 >> f2

instance Renderable (Segment R2) (SunroofBackend t) where
  render _ (Linear v) = SRender $ lineTo' (unr2 v)
  render _ (Cubic c1 c2 v) = SRender $ bezierCurveTo' (unr2 c1) (unr2 c2) (unr2 v)

instance Renderable (Trail R2) (SunroofBackend t) where
  render _ (Trail segs c) = SRender $ do
    mapM_ (unRender . render SunroofBackend) segs
    when c $ r $ SR.closePath

instance Renderable (Path R2) (SunroofBackend t) where
  render :: SunroofBackend t -> Path R2 -> Render (SunroofBackend t) (V (Path R2))
  render _ (Path trs) = SRender $ do
    r $ SR.beginPath
    mapM_ renderTrail trs
      where renderTrail :: (Point R2, Trail R2) -> CanvasM t ()
            renderTrail (p, tr) = do
              moveTo' $ unp2 p
              unRender $ render SunroofBackend tr

instance Renderable Text (SunroofBackend t) where
  render :: SunroofBackend t -> Text -> Render (SunroofBackend t) (V Text)
  render b (Text trans align text) = withStyle b mempty trans $ SRender $ do
      r $ SR.fillText (js text) (0, 0) -- TODO

-- -----------------------------------------------------------------------
-- Utility Functions
-- -----------------------------------------------------------------------

unRender :: (Backend b v, b ~ SunroofBackend t, v ~ R2) => Render b v -> CanvasM t ()
unRender (SRender r) = r

setCanvasTrans :: Transformation R2 -> JSRenderOp t
setCanvasTrans t c = c # SR.transform (js a1) (js a2) (js b1) (js b2) (js c1) (js c2)
  where (a1,a2) = unr2 $ apply t unitX
        (b1,b2) = unr2 $ apply t unitY
        (c1,c2) = unr2 $ transl t

setCanvasStyle :: forall v t. Style v -> CanvasM t ()
setCanvasStyle s = foldr (>>) (return ()) $ catMaybes $ 
    [ handle $ fillColor' . getFillColor
    , handle $ strokeColor' . getLineColor
    , handle $ lineWidth' . getLineWidth
    , handle $ lineJoin' . getLineJoin
    , handle $ lineCap' . getLineCap
    , handle $ globalAlpha' . getOpacity
    ]
  where handle :: (AttributeClass a) => (a -> CanvasM t ()) -> Maybe (CanvasM t ())
        handle f = f `fmap` getAttr s

strokeColor' :: (Color c) => c -> CanvasM t ()
strokeColor' c = r $ SR.strokeStyle := (js $ showColorJS c)

fillColor' :: (Color c) => c -> CanvasM t ()
fillColor' c = r $ SR.setFillStyle $ js $ showColorJS c

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
      (r,g,b,a) = colorToRGBA c
  in concat [ "rgba(", s r, ",", s g, ",", s b, ",", show a, ")" ]

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
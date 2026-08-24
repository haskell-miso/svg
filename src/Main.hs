-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict    as M
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
import qualified Miso.Html.Element  as H
import           Miso.Html.Event    (onClick, onInput)
import qualified Miso.Html.Property as P
import qualified Miso.Svg.Element   as S
import qualified Miso.Svg.Property  as SP
import           Miso.JSON          (withArray, withObject, (.:))
import           Miso.String        (MisoString, ms, fromMisoString)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
-- | Model: animation clock, spirograph + lissajous parameters, pointer trail
data Model = Model
  { _millis :: Double
  , _bigR   :: Int
  , _smallR :: Int
  , _dist   :: Int
  , _hue    :: Int
  , _freqA  :: Int
  , _freqB  :: Int
  , _trail  :: [(Double, Double)]
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
millis :: Lens Model Double
millis = lens _millis $ \m x -> m { _millis = x }

bigR, smallR, dist, hue, freqA, freqB :: Lens Model Int
bigR   = lens _bigR   $ \m x -> m { _bigR   = x }
smallR = lens _smallR $ \m x -> m { _smallR = x }
dist   = lens _dist   $ \m x -> m { _dist   = x }
hue    = lens _hue    $ \m x -> m { _hue    = x }
freqA  = lens _freqA  $ \m x -> m { _freqA  = x }
freqB  = lens _freqB  $ \m x -> m { _freqB  = x }

trail :: Lens Model [(Double, Double)]
trail = lens _trail $ \m x -> m { _trail = x }
-----------------------------------------------------------------------------
data Action
  = Tick Double
  | SetBigR Int
  | SetSmallR Int
  | SetDist Int
  | SetHue Int
  | SetFreqA Int
  | SetFreqB Int
  | Pointer (Double, Double)
  | ClearTrail
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = Model 0 84 39 65 280 3 4 []
-----------------------------------------------------------------------------
main :: IO ()
main = startApp events app
  where
    events :: Events
    events = M.insert "pointermove" BUBBLE defaultEvents
-----------------------------------------------------------------------------
app :: App Model Action
app = (component emptyModel updateModel viewModel)
  { subs = [ rAFSub Tick ]
  }
-----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  Tick t      -> millis .= t
  SetBigR n   -> bigR .= n
  SetSmallR n -> smallR .= n
  SetDist n   -> dist .= n
  SetHue n    -> hue .= n
  SetFreqA n  -> freqA .= n
  SetFreqB n  -> freqB .= n
  Pointer p   -> trail %= \ps -> take 80 (p : ps)
  ClearTrail  -> trail .= []
-----------------------------------------------------------------------------
viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ m =
  H.div_
  [ P.class_ "app" ]
  [ H.header_
    [ P.class_ "hero" ]
    [ H.h1_ [] [ "🍜 🖼️ ", H.a_ [ P.href_ repoUrl ] [ "miso-svg" ] ]
    , H.p_ [ P.class_ "tagline" ]
      [ "Interactive, animated SVG rendered from pure Haskell views — "
      , "no JavaScript written by hand."
      ]
    , H.a_ [ P.class_ "gh", P.href_ repoUrl ] [ "View source on GitHub" ]
    ]
  , H.main_
    [ P.class_ "grid" ]
    [ spiroCard m
    , lissajousCard m
    , trailCard m
    ]
  , H.footer_
    [ P.class_ "foot" ]
    [ H.p_ []
      [ "Built with "
      , H.a_ [ P.href_ "https://github.com/dmjio/miso" ] [ "miso" ]
      , ", a Haskell web framework — compiled to WebAssembly."
      ]
    ]
  ]
  where
    repoUrl = "https://github.com/haskell-miso/miso-svg"
-----------------------------------------------------------------------------
-- | Hypotrochoid ("spirograph") — the path is rebuilt from the model on
-- every render, demonstrating dynamic attribute generation.
spiroCard :: Model -> View () Model Action
spiroCard m =
  H.section_
  [ P.class_ "card" ]
  [ H.h2_ [] [ "Spirograph" ]
  , H.p_ [ P.class_ "hint" ]
    [ "A hypotrochoid traced by a point attached to a circle rolling inside "
    , "another circle. Drag the sliders — the SVG path is recomputed in Haskell."
    ]
  , S.svg_
    [ SP.viewBox_ "0 0 400 400", P.class_ "stage" ]
    [ S.g_
      [ SP.transform_ "translate(200,200)" ]
      [ S.path_
        [ SP.d_ (spiroPath (m ^. bigR) (m ^. smallR) (m ^. dist))
        , SP.fill_ "none"
        , SP.stroke_ (hsl (m ^. hue) 60)
        , SP.strokeWidth_ "1.5"
        , SP.strokeLinecap_ "round"
        ]
      ]
    ]
  , H.div_
    [ P.class_ "controls" ]
    [ slider "Outer radius" 20 190 (m ^. bigR) SetBigR
    , slider "Inner radius" 2 120 (m ^. smallR) SetSmallR
    , slider "Pen distance" 1 160 (m ^. dist) SetDist
    , slider "Hue" 0 360 (m ^. hue) SetHue
    ]
  ]
-----------------------------------------------------------------------------
-- | Lissajous curve, phase-animated with 'rAFSub'.
lissajousCard :: Model -> View () Model Action
lissajousCard m =
  H.section_
  [ P.class_ "card" ]
  [ H.h2_ [] [ "Lissajous curves" ]
  , H.p_ [ P.class_ "hint" ]
    [ "Harmonic motion on both axes, with the phase driven by a "
    , "requestAnimationFrame subscription (", H.code_ [] [ "rAFSub" ], ")."
    ]
  , S.svg_
    [ SP.viewBox_ "0 0 400 400", P.class_ "stage" ]
    [ S.g_
      [ SP.transform_ "translate(200,200)" ]
      [ S.polyline_
        [ SP.points_ (lissajousPoints a b phase)
        , SP.fill_ "none"
        , SP.stroke_ (hsl (m ^. hue + 60) 55)
        , SP.strokeWidth_ "1.5"
        ]
      , S.circle_
        [ SP.cx_ (f1 hx)
        , SP.cy_ (f1 hy)
        , SP.r_ "7"
        , SP.fill_ (hsl (m ^. hue + 60) 75)
        ]
      ]
    ]
  , H.div_
    [ P.class_ "controls" ]
    [ slider "Frequency a" 1 9 a SetFreqA
    , slider "Frequency b" 1 9 b SetFreqB
    ]
  ]
  where
    a = m ^. freqA
    b = m ^. freqB
    phase = (m ^. millis) / 1600
    (hx, hy) = lissajousPoint a b phase phase
-----------------------------------------------------------------------------
-- | Pointer trail — a custom event 'Decoder' maps pointer coordinates into
-- viewBox space, so it works at any rendered size, mouse or touch.
trailCard :: Model -> View () Model Action
trailCard m =
  H.section_
  [ P.class_ "card wide" ]
  [ H.h2_ [] [ "Pointer trail" ]
  , H.p_ [ P.class_ "hint" ]
    [ "Move your pointer (or finger) across the panel. A custom event decoder "
    , "reads offset and element size to stay accurate at any scale."
    ]
  , S.svg_
    [ SP.viewBox_ "0 0 800 400"
    , P.class_ "stage paint"
    , on "pointermove" (offsetDecoder 800 400) (\p _ _ -> Pointer p)
    ]
    ( trailBackdrop
    : [ S.circle_
        [ SP.cx_ (f1 x)
        , SP.cy_ (f1 y)
        , SP.r_ (f1 (3 + 22 * age i))
        , SP.fill_ "none"
        , SP.stroke_ (hsl (m ^. hue + i * 3) 60)
        , SP.strokeWidth_ "2"
        , SP.strokeOpacity_ (f1 (age i))
        ]
      | (i, (x, y)) <- zip [0 ..] (m ^. trail)
      ]
    )
  , H.div_
    [ P.class_ "controls" ]
    [ H.button_ [ P.class_ "btn", onClick ClearTrail ] [ "Clear" ] ]
  ]
  where
    total = 80 :: Int
    age i = fromIntegral (total - i) / fromIntegral total :: Double
    trailBackdrop =
      S.text_
      [ SP.x_ "400", SP.y_ "205", P.class_ "watermark"
      , SP.textAnchor_ "middle"
      ]
      [ text (if null (m ^. trail) then "draw here" else "") ]
-----------------------------------------------------------------------------
-- | Decode @(offsetX, offsetY)@ scaled into a @viewBox@ coordinate system.
offsetDecoder :: Double -> Double -> Decoder (Double, Double)
offsetDecoder vw vh = Decoder
  { decodeAt = DecodeTargets [ [], [ "target" ] ]
  , decoder = withArray "pointer" $ \case
      [ ev, tgt ] -> do
        (ox, oy) <- flip (withObject "event") ev $ \o ->
          (,) <$> o .: "offsetX" <*> o .: "offsetY"
        (cw, ch) <- flip (withObject "target") tgt $ \o ->
          (,) <$> o .: "clientWidth" <*> o .: "clientHeight"
        pure (vw * ox / max 1 cw, vh * oy / max 1 ch)
      _ -> fail "expected [event, target]"
  }
-----------------------------------------------------------------------------
spiroPath :: Int -> Int -> Int -> MisoString
spiroPath bigR' smallR' dist' =
  case points of
    []           -> ""
    (x, y) : ps  ->
      "M " <> f1 x <> " " <> f1 y
           <> mconcat [ " L " <> f1 px <> " " <> f1 py | (px, py) <- ps ]
  where
    rr = fromIntegral bigR'
    sr = fromIntegral (max 1 smallR')
    dd = fromIntegral dist'
    loops = max 1 (max 1 smallR' `div` gcd (max 1 bigR') (max 1 smallR'))
    n = min 2200 (loops * 130) :: Int
    k = (rr - sr) / sr
    pt i =
      let t = 2 * pi * fromIntegral loops * fromIntegral i / fromIntegral n
      in ( (rr - sr) * cos t + dd * cos (k * t)
         , (rr - sr) * sin t - dd * sin (k * t)
         )
    points = map pt [ 0 .. n ]
-----------------------------------------------------------------------------
lissajousPoint :: Int -> Int -> Double -> Double -> (Double, Double)
lissajousPoint a b phase t =
  ( 170 * sin (fromIntegral a * t + phase)
  , 170 * sin (fromIntegral b * t)
  )
-----------------------------------------------------------------------------
lissajousPoints :: Int -> Int -> Double -> MisoString
lissajousPoints a b phase = mconcat
  [ f1 x <> "," <> f1 y <> " "
  | i <- [ 0 .. n ]
  , let t = 2 * pi * fromIntegral i / fromIntegral n
  , let (x, y) = lissajousPoint a b phase t
  ]
  where
    n = 360 :: Int
-----------------------------------------------------------------------------
-- | A labeled range slider bound to an 'Int' field of the model.
slider :: MisoString -> Int -> Int -> Int -> (Int -> Action) -> View () Model Action
slider label lo hi val toAction =
  H.label_
  [ P.class_ "slider" ]
  [ H.span_ [ P.class_ "slider-name" ] [ text label ]
  , H.input_
    [ P.type_ "range"
    , P.min_ (ms lo)
    , P.max_ (ms hi)
    , P.step_ "1"
    , P.value_ (ms val)
    , onInput (toAction . fromMisoString)
    ]
  , H.span_ [ P.class_ "slider-val" ] [ text (ms val) ]
  ]
-----------------------------------------------------------------------------
hsl :: Int -> Int -> MisoString
hsl h l = "hsl(" <> ms (h `mod` 360) <> ", 85%, " <> ms l <> "%)"
-----------------------------------------------------------------------------
-- | Render a 'Double' with one decimal place (keeps SVG attributes small).
f1 :: Double -> MisoString
f1 x = ms (fromIntegral (round (x * 10) :: Int) / 10 :: Double)
-----------------------------------------------------------------------------

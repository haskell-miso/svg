-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad.State
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso hiding (update, text_)
import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
import           Miso.String (ms)
import           Miso.Lens (this, (.=))
import           Miso.Svg hiding (height_, id_, style_, width_)
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
#if WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = startApp (M.singleton "pointermove" BUBBLE) app
-----------------------------------------------------------------------------
-- | Component definition (uses 'component' smart constructor)
app :: App Model Action
app = (component emptyModel updateModel viewModel)
  { subs = [ mouseSub HandlePointer ]
  }
-----------------------------------------------------------------------------
emptyModel :: Model
emptyModel = (0, 0)
-----------------------------------------------------------------------------
updateModel :: Action -> Effect parent Model Action
updateModel (HandlePointer pointer) = this .= client pointer
-----------------------------------------------------------------------------
data Action = HandlePointer PointerEvent
-----------------------------------------------------------------------------
type Model = (Double, Double)
-----------------------------------------------------------------------------
viewModel :: props -> Model -> View Model Action
viewModel _ (x, y) =
  vfrag
    [ H.h1_ 
      [ CSS.style_ [ CSS.fontFamily "monospace" ] 
      ]
      [ "🍜 🖼️ "
      , H.a_ [ P.href_ "https://github.com/haskell-miso/miso-svg" ] [ "miso-svg" ] 
      ]
    , H.svg_
      [ CSS.style_
        [ CSS.borderStyle "solid"
        , CSS.height "700px"
        , CSS.width "100%"
        ]
      ]
      [ S.g_
        []
        [ S.ellipse_
          [ SP.cx_ (ms x)
          , SP.cy_ (ms y)
          , CSS.style_
            [ CSS.fill "yellow"
            , CSS.stroke "purple"
            , CSS.strokeWidth "2"
            ]
          , SP.rx_ "100"
          , SP.ry_ "100"
          ]
        ]
      , text_
        [ SP.x_ (ms x)
        , SP.y_ (ms y)
        ]
        [ text $ ms $ show (round x, round y)
        ]
      ]
    ]
-----------------------------------------------------------------------------

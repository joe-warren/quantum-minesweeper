module App.Game
  ( Action(..)
  , State
  , component
  , render
  )
  where

import Prelude
import App.Grid (Grid, Coordinates, Square (..))
import App.Grid as Grid
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import Data.Int as Int
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Icons as Icons

type State
  = { board :: Grid Square 
    , minecount :: Int
    }

data Action
  = Clear Coordinates
  | Check Coordinates
  | Qntm

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { board : Grid.empty 16 16, minecount : 30 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

cellSize :: Number
cellSize = 32.0


renderSquare :: forall cs m. Coordinates -> Square -> H.ComponentHTML Action cs m
renderSquare c square = 
  HSE.g
    [ HSA.transform [HSA.Translate (cellSize * Int.toNumber c.x) (cellSize * Int.toNumber c.y)]
    , HE.onClick $ \_ -> Check c
    ]
    [ HSE.rect [HSA.width cellSize, HSA.height cellSize, HSA.class_ (HH.ClassName "background")]
    , case square of
        UnrevealedSquare -> Icons.iconUnclicked []
        FlaggedSquare -> Icons.iconFlag []
        _ -> HSE.g [] []
    ]

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  let sw = cellSize * (Int.toNumber ((Grid.size state.board).width))
      sh = cellSize * (Int.toNumber ((Grid.size state.board).height))
  in HH.div_
    [ HSE.svg 
        [ HSA.viewBox 0.0 0.0  sw sh
        , HSA.class_ (HH.ClassName "gameboard")
        ]
        (Array.fromFoldable (mapWithIndex renderSquare state.board))
    , HH.button
        [ HE.onClick \_ -> Qntm ]
        [ HH.text "Click me" ]
    ]

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Check coords -> H.modify_ \st -> st {board = Grid.updateAt' coords FlaggedSquare st.board}
  _ -> H.modify_ \st -> st

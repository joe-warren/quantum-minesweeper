module App.Game
  ( Action(..)
  , Command(..)
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
import Handlers as Handlers
import Web.UIEvent.MouseEvent as MouseEvent
import Web.Event.Event (Event, preventDefault)
import Data.Maybe
import Data.Traversable (traverse_)
import Effect.Class (class MonadEffect, liftEffect)

type State
  = { board :: Grid Square 
    , minecount :: Int
    }

data Command
  = Clear Coordinates
  | Flag Coordinates
  | Qntm

data Action = Action (Maybe Event) Command

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { board : Grid.empty 16 16, minecount : 30 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

cellSize :: Number
cellSize = 32.0


click :: Coordinates -> MouseEvent.MouseEvent -> Command
click c ev = 
  case MouseEvent.button ev of
    0 -> if MouseEvent.metaKey ev || MouseEvent.ctrlKey ev
          then Flag c
          else Clear c
    _ -> Flag c


renderSquare :: forall cs m. Coordinates -> Square -> H.ComponentHTML Action cs m
renderSquare c square = 
  HSE.element (HH.ElemName "g")
    [ HSA.transform [HSA.Translate (cellSize * Int.toNumber c.x) (cellSize * Int.toNumber c.y)]
    , HE.onClick $ Action Nothing <<< click c
    , Handlers.onContextMenu \ev -> Action (Just ev ) (Flag c)
    ]
    [ HSE.rect [HSA.width cellSize, HSA.height cellSize, HSA.class_ (HH.ClassName "background")]
    , case square of
        UnrevealedSquare -> Icons.iconUnclicked []
        FlaggedSquare -> Icons.iconFlag []
        RevealedSquare i | i > 0 -> 
          HSE.text 
            [ HSA.classes 
              [ HH.ClassName "revealed-square"
              , HH.ClassName ( "value-" <> show i )
              ]
            , HSA.x (cellSize / 2.0)
            , HSA.y (cellSize / 2.0)
            ]
            [ HH.text (show i) ]
        _ -> HSE.g [] []
    ]

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  let sw = cellSize * (Int.toNumber ((Grid.size state.board).width))
      sh = cellSize * (Int.toNumber ((Grid.size state.board).height))
  in HH.div
    [HP.class_ (HH.ClassName "content")]
    [ HH.button
        [ HE.onClick \_ -> Action Nothing Qntm ]
        [ HH.text "Reveal A Square" ]
    , HH.br []
    , HSE.svg 
        [ HSA.viewBox 0.0 0.0  sw sh
        , HSA.class_ (HH.ClassName "gameboard")
        ]
        (Array.fromFoldable (mapWithIndex renderSquare state.board))
    ]

flag :: Coordinates -> State -> State
flag c st = 
  let f FlaggedSquare = UnrevealedSquare
      f UnrevealedSquare = FlaggedSquare
      f x = x
  in st { board = Grid.modifyAt' c f st.board }

clear :: Coordinates -> State -> State
clear c st = 
  let f UnrevealedSquare = RevealedSquare 0
      f (RevealedSquare i) = RevealedSquare (i + 1)
      f x = x
  in st { board = Grid.modifyAt' c f st.board }


handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction (Action mayEv command) =
  do 
    liftEffect $ traverse_ preventDefault mayEv
    case command of
      Flag coords -> H.modify_ (flag coords)
      Clear coords -> H.modify_ (clear coords)
      _ -> H.modify_ \st -> st

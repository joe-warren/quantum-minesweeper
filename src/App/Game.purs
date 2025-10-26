module App.Game
  ( Action(..)
  , Command(..)
  , State
  , component
  , handleAction
  )
  where

import Prelude

import App.Grid (Grid, Coordinates)
import App.Grid as Grid
import App.Square as Square
import App.Square (Square)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Svg.Elements as HSE
import Halogen.Svg.Attributes as HSA
import Data.Int as Int
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Foldable (sum, foldr, any, all)
import Icons as Icons
import Handlers as Handlers
import Web.UIEvent.MouseEvent as MouseEvent
import Web.Event.Event (Event, preventDefault)
import Data.Maybe
import Data.Traversable (traverse_)
import Effect.Class (class MonadEffect, liftEffect)

type State
  = { board :: Grid Square 
    , deaths :: Int
    }

data Command
  = Clear Coordinates
  | Flag Coordinates
  | Qntm
  | Undo

data Action = Action (Maybe Event) Command

component :: forall q i o m. MonadEffect m => m (H.Component q i o m)
component = do
  g <- liftEffect $ Grid.randomGrid 16 16 40

  pure $ H.mkComponent
    { initialState: \_ -> { board : g, deaths: 0 }
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

minecount :: Grid Square -> Int
minecount = 
    let f s = if Square.Mined == Square.isMined s then 1 else 0
      in sum <<< map f

      
flagcount :: Grid Square -> Int
flagcount = 
    let f (Square.Flagged _) = 1
        f _ = 0
      in sum <<< map f

solved :: Grid Square -> Boolean
solved = 
  let f (Square.Revealed _) = true
      f s = Square.Mined == Square.isMined s
    in all f

    
failed :: Grid Square -> Boolean
failed = 
  let f (Square.Exploded) = true
      f _ = false
    in any f

renderSquare :: forall cs m. Coordinates -> Square -> H.ComponentHTML Action cs m
renderSquare c square = 
  HSE.element (HH.ElemName "g")
    [ HSA.transform [HSA.Translate (cellSize * Int.toNumber c.x) (cellSize * Int.toNumber c.y)]
    , HE.onClick $ Action Nothing <<< click c
    , Handlers.onContextMenu \ev -> Action (Just ev ) (Flag c)
    ]
    [ HSE.rect [HSA.width cellSize, HSA.height cellSize, HSA.class_ (HH.ClassName "background")]
    , case square of
        Square.Unrevealed _ -> Icons.iconUnclicked []
        Square.Exploded -> Icons.iconMine []
        Square.Flagged _ -> Icons.iconFlag []
        Square.Revealed i | i > 0 -> 
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
    , if failed state.board
        then HH.button
          [ HE.onClick \_ -> Action Nothing Undo ]
          [ HH.text "Undo" ]
        else HH.text ""
    , HH.br []
    , HSE.svg 
        [ HSA.viewBox 0.0 0.0  sw sh
        , HSA.class_ (HH.ClassName "gameboard")
        ]
        (Array.fromFoldable (mapWithIndex renderSquare state.board))
    , HH.p 
      []
      [HH.text (show (flagcount state.board) <> "/" <> show (minecount state.board) <> " mines")
      , case state.deaths of
          0 -> HH.text ""
          1 -> HH.text ", 1 death"
          deaths -> HH.text (", " <> show deaths <> " deaths")
      ]
    
    ]

flag :: Coordinates -> State -> State
flag c st = 
  let f (Square.Flagged isMined) = Square.Unrevealed isMined
      f (Square.Unrevealed isMined) = Square.Flagged isMined
      f x = x
  in if failed (st.board)
        then st
        else st { board = Grid.modifyAt' c f st.board }



countNeighbourMines :: Coordinates -> Grid Square -> Int
countNeighbourMines = 
  let f = (_ == Square.Mined ) <<< Square.isMined
  in Grid.countNeighbours f


countNeighbourFlags :: Coordinates -> Grid Square -> Int
countNeighbourFlags = 
  let f (Square.Flagged _) = true
      f _ = false 
  in Grid.countNeighbours f

clear :: Coordinates -> State -> State
clear c st = 
  let clearNeighbours st' =
        let unrevealed (Just (Square.Unrevealed _)) = true
            unrevealed _ = false
            filterRevealed = Array.filter (unrevealed <<< Grid.index st'.board)
         in foldr clear st' (filterRevealed <<< Grid.neighbours $ c)
  in if failed (st.board)
      then st
      else 
        case Grid.index (st.board) c of 
            Nothing -> st 
            Just (Square.Flagged _) -> st
            Just (Square.Revealed count) ->
              if count == countNeighbourFlags c st.board 
                then clearNeighbours st
                else st
            Just s -> 
              case Square.isMined s of 
                Square.Mined -> st { board = Grid.updateAt' c Square.Exploded st.board }
                Square.Unmined ->
                  let count = countNeighbourMines c st.board
                      newSt = st { board = Grid.updateAt' c (Square.Revealed count) st.board }
                  in if count == 0 
                        then clearNeighbours newSt
                        else newSt

undo :: State -> State
undo st =
  let reset Square.Exploded = Square.Unrevealed Square.Mined
      reset s = s
  in st { board = map reset st.board
        , deaths = st.deaths + 1
        }

handleAction :: forall cs o m. MonadEffect m => Action → H.HalogenM State Action cs o m Unit
handleAction (Action mayEv command) =
  do 
    liftEffect $ traverse_ preventDefault mayEv
    case command of
      Flag coords -> H.modify_ (flag coords)
      Clear coords -> H.modify_ (clear coords)
      Undo -> H.modify_ (undo)
      _ -> H.modify_ \st -> st

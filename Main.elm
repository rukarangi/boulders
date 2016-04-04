import Text exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard exposing (arrows, keysDown)
import Signal exposing (foldp)
import Char exposing (fromCode)
import Time exposing (fps)
import String

main : Signal Element
main = Signal.map view (foldp update init updates)

type alias State = 
  { carx : Float 
  , moving : Float
  }

init = { moving = 0.0, carx = 0.0 }

--update : Update -> State -> State
update up st =
  case up of 
    TimeDelta t -> let c = st.carx in { st | carx = c + st.moving * 1 } 
    LeftArrow i -> {st | moving = toFloat i }

type Update = TimeDelta Float | LeftArrow Int 

leftArrow : Signal Int
leftArrow = Signal.map (.x) arrows

updates = Signal.merge 
              (Signal.map LeftArrow leftArrow)
              (Signal.map TimeDelta (fps 400))

view : State -> Element
view st = header `above` (board st.carx)


header : Element
header = 
  let  h = fromString "Luke's boulder game"
            |> Text.color red           
            |> Text.height 40
            |> Text.bold
  in container 1500 100 middle (centered h)

-------------------------------------------------------
-- Board
-------------------------------------------------------

board : Float -> Element
board carx = collage 1500 500 
          [ rect 800 500 |> filled clearGrey
          , car |> move (carx, -200) ] 

--------------------------------
-- Car 
--------------------------------

car : Form
car = 
  group
    [ filled carBlue (oval 40 75)
    , filled carBlack (rect 15 15) |> move (25, 25)
    , filled carBlack (rect 15 15) |> move (-25, 25)
    , filled carBlack (rect 15 15) |> move (25, -25)
    , filled carBlack (rect 15 15) |> move (-25, -25)
    ]


clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6

carBlack : Color
carBlack =
  rgba 25 25 25 1

carBlue : Color
carBlue =
  rgba 0 0 255 1

type alias Car 
    = { }

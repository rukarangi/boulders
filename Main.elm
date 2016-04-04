import Text exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard exposing (arrows, keysDown)
import Signal exposing (foldp)
import Time exposing (fps)

main : Signal Element
main = Signal.map view (foldp update init updates)

type alias State = 
  { carx : Float 
  , moving : Float
  }

init : State
init = { moving = 0.0, carx = 0.0 }

update : Update -> State -> State
update up st =
  case up of 
    TimeDelta _ -> let c = st.carx in { st | carx = updateX c st.moving } 
    SideArrow i -> {st | moving = toFloat i }

updateX : Float -> Float -> Float
updateX x d = 
    let x' = x + d * 3
    in  if x' > 410.0
          then -410.0
          else if x' < -410.0
                 then 410.0 
                 else x'


type Update = TimeDelta Float | SideArrow Int

sideArrow : Signal Int
sideArrow = Signal.map (.x) arrows

updates : Signal Update
updates = Signal.merge 
              (Signal.map SideArrow sideArrow)
              (Signal.map TimeDelta (fps 100))

view : State -> Element
view st = header `above` (board st.carx)


header : Element
header = 
  let  h = fromString "Luke's boulder game"
            |> Text.color red           
            |> Text.height 40
            |> Text.bold
  in container 1200 100 middle (centered h)

-------------------------------------------------------
-- Board
-------------------------------------------------------

board : Float -> Element
board carx = collage 1200 500 
          [ rect 800 500 |> filled clearGrey
          , car |> move (carx, -200) ] 

--------------------------------
-- Car 
--------------------------------

car : Form
car = toForm <| image 50 75 "sprites/car_number_one.png"


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

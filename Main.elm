import Text exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard exposing (arrows, keysDown, wasd)
import Signal exposing (foldp)
import Time exposing (fps, Time)
import Random as R

speedOfBoulders : Float
speedOfBoulders = 2

main : Signal Element
main = Signal.map view (foldp update init updates)

type alias State = 
  { carx : Float 
  , moving : Float
  , boulders : List (Float, Float)
  }

init : State
init = 
  { moving = 0.0
  , carx = 0.0
  , boulders = []
  }

update : Update -> State -> State
update up st =
  case up of
    SideArrow i -> {st | moving = toFloat i } 
    TimeDelta t -> 
      let c = st.carx
          b = st.boulders 
      in { st | carx = updateX c st.moving 
              , boulders = updateboulders t b} 
    

updateX : Float -> Float -> Float
updateX x d = 
    let x' = x + d * 3
    in  if x' > 378.0
          then -378.0
          else if x' < -378.0
                 then 378.0 
                 else x'

updateboulders : Time -> List (Float, Float) -> List (Float, Float)
updateboulders t b = 
  case b of
    [] ->
      let s = R.initialSeed (floor (Time.inMilliseconds t))
          (number_of_boulders, s') = R.generate (R.int 2 4) s
          makeX se bs = 
            case bs of
              b::bbs ->  
                let (x, se') = R.generate (R.int -378 378) se
                in  (toFloat x, snd b) :: makeX se' bbs
              [] -> [] 
      in makeX s' (List.repeat number_of_boulders (0,270))
    bs -> 
      let moveDown b = 
            if snd b < -270
              then Nothing 
              else Just (fst b, snd b - speedOfBoulders)
      in List.filterMap moveDown bs

type Update = TimeDelta Float | SideArrow Int

sideArrow : Signal Int
sideArrow = Signal.map (.x) (Signal.merge arrows wasd)

updates : Signal Update
updates = Signal.merge 
              (Signal.map SideArrow sideArrow)
              (Signal.map TimeDelta (fps 100))

---------------------------------------------------------
-- View
---------------------------------------------------------

view : State -> Element
view st = header `above` (board st)


header : Element
header = 
  let  h = fromString "Luke's Boulder Game"
            |> Text.color red           
            |> Text.height 40
            |> Text.bold
            |> Text.italic
  in container 1200 100 middle (centered h)

-------------------------------------------------------
-- Board
-------------------------------------------------------

board : State -> Element
board st = 
  let bs = List.map makeBoulder st.boulders
      makeBoulder pos = boulderone |> move pos 
  in collage 1200 500 <|
          [ rect 800 500 |> filled clearGrey
          , car |> move (st.carx, -200)
          ] `List.append` bs 

--------------------------------
-- Car 
--------------------------------

car : Form
car = toForm <| image 50 75 "sprites/car_number_one.png"

boulderone : Form
boulderone = toForm <| image 100 100 "sprites/boulder_one.png"

rockone : Form
rockone = toForm <| image 100 100 "sprites/rock_one.png"

rocktwo : Form
rocktwo = toForm <| image 100 100 "sprites/rock_two.png"

clearGrey : Color
clearGrey =
  rgba 235 153 45 1

carBlack : Color
carBlack =
  rgba 25 25 25 1

carBlue : Color
carBlue =
  rgba 0 0 255 1

type alias Car 
    = { }

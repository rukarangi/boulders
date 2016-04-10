import Text exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard exposing (arrows, keysDown, wasd)
import Signal exposing (foldp)
import Time exposing (fps, Time)
import Random as R
import Basics exposing (toString)

main : Signal Element
main = Signal.map view (foldp update init updates)

type alias State = 
  { carx : Float 
  , moving : Float
  , boulders : List (Float, Float)
  , bouldersp : Float
  , score : Float
  , lastSeen : Float
  , pause : Bool
  }

init : State
init = 
  { moving = 0.0
  , carx = 0.0
  , boulders = []
  , bouldersp = 2
  , score = 0.0
  , lastSeen = 0.0
  , pause = True 
  }

update : Update -> State -> State
update up st =
  case up of
    TogglePlay -> let p = st.pause in {st | pause = not p, moving = 0.0}
    SideArrow i -> if st.pause then st else {st | moving = toFloat i } 
    TimeDelta t -> 
      if st.pause 
        then st 
        else
          let c = st.carx
              stateo = updateboulders t st
              stsc = updateScore stateo
          in { stsc | carx = updateX c st.moving } 
    

updateX : Float -> Float -> Float
updateX x d = 
    let x' = x + d * 3
    in  if x' > 378.0
          then -378.0
          else if x' < -378.0
                 then 378.0 
                 else x'

updateScore : State -> State
updateScore st = 
      let sc = st.score
          ls = st.lastSeen
          inc = if by > ls then 1 else 0
          by = case st.boulders of
                [] -> st.lastSeen
                ( b :: _ ) -> snd b
      in { st | score = sc + inc
              ,lastSeen = by } 

updateboulders : Time -> State -> State
updateboulders t st =
  case st.boulders of
    [] ->
      let s = R.initialSeed (floor (Time.inMilliseconds t))
          (number_of_boulders, s') = R.generate (R.int 2 4) s
          makeX se bs = 
            case bs of
              b::bbs ->  
                let (x, se') = R.generate (R.int -378 378) se
                in  (toFloat x, snd b) :: makeX se' bbs
              [] -> []
          sp = st.bouldersp
      in { st | boulders = makeX s' (List.repeat number_of_boulders (0,270))
              , bouldersp = sp + 1 }
    bs -> 
      let moveDown b = 
            if snd b < -270
              then Nothing 
              else Just (fst b, snd b - st.bouldersp)
          ns = if st.bouldersp > 10 then 1 else st.bouldersp
      in { st | boulders = List.filterMap moveDown bs
              ,bouldersp = ns }


type Update = TimeDelta Float | SideArrow Int | TogglePlay 

sideArrow : Signal Int
sideArrow = Signal.map (.x) (Signal.merge arrows wasd)

updates : Signal Update
updates = Signal.mergeMany 
              [ (Signal.map SideArrow sideArrow)
              , (Signal.map TimeDelta (fps 100))
              , (Signal.map (always TogglePlay) (Signal.filter identity False Keyboard.space)) ]

---------------------------------------------------------
-- View
---------------------------------------------------------

view : State -> Element
view st = header `above` (scorestr  st) `above` (instructions st) `above` (board st)

scorestr : State -> Element
scorestr st = 
  let scr = fromString (toString st.score)
            |> Text.color yellow
            |> Text.height 15
            |> Text.bold
  in container 1200 50 middle (centered scr)

instructions : State -> Element
instructions st = 
  let inst = fromString ("a - d or <- - -> to move and space to " ++ (if st.pause then "play" else "pause"))
            |> Text.color blue
            |> Text.height 15
            |> Text.bold
  in container 1200 15 middle (centered inst)

header : Element
header = 
  let  h = fromString "Luke's Boulder Game"
            |> Text.color red           
            |> Text.height 40
            |> Text.bold
            |> Text.italic
  in container 1200 50 middle (centered h)

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

ss : State -> String
ss st = toString st.score

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
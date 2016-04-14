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
  , ingame : Bool
  , hScore : Float
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
  , ingame = False
  , hScore = 0.0
  }

update : Update -> State -> State
update up st =
  case up of
    TogglePlay -> updatePlay st
    SideArrow i -> if st.pause then st else {st | moving = toFloat i } 
    TimeDelta t -> 
      if st.pause 
        then st 
        else
          let c = st.carx
              stateo = updateboulders t st
              stsc = updateScore stateo
              isc = updateCollision stsc
          in { isc | carx = updateX c st.moving } 
    

updatePlay : State -> State
updatePlay st =
    case ( st.pause, st.ingame ) of
      (True, True) -> { st | pause = False}
      (False, True) -> { st | pause = True, moving = 0.0}
      (True, False) -> { init | pause = False
                              , ingame = True
                              , hScore = (if st.score > st.hScore then 
                                            st.score else 
                                            st.hScore)}

      (False, False) -> { init | pause = False, ingame = True}

updateX : Float -> Float -> Float
updateX x d = 
    let x' = x + d * 9
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
              , lastSeen = by } 

updateCollision : State -> State
updateCollision st = 
  if isCollision st 
    then { st | ingame = False, pause = True}
    else st

isCollision : State -> Bool
isCollision st = 
  let carhit b = 
          let bty = (snd b + 50)
              bby = (snd b - 50)
              bxl = (fst b - 40)
              bxr = (fst b + 40)
          in bty > -200 && bby < -200 && bxl < st.carx && bxr > st.carx
  in List.any carhit st.boulders

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
              , bouldersp = sp + 2 }
    bs -> 
      let moveDown b = 
            if snd b < -270
              then Nothing 
              else Just (fst b, snd b - st.bouldersp)
          ns = if st.bouldersp > 15 then 2 else st.bouldersp
      in { st | boulders = List.filterMap moveDown bs
              ,bouldersp = ns }


type Update = TimeDelta Float | SideArrow Int | TogglePlay 

sideArrow : Signal Int
sideArrow = Signal.map (.x) (Signal.merge arrows wasd)

updates : Signal Update
updates = Signal.mergeMany 
              [ (Signal.map SideArrow sideArrow)
              , (Signal.map TimeDelta (fps 40))
              , (Signal.map (always TogglePlay) (Signal.filter identity False Keyboard.space)) ]

---------------------------------------------------------
-- View
---------------------------------------------------------

view : State -> Element
view st = header `above` (hscorestr st) `above` (scorestr  st) `above` (instructions st) `above` (board st)

hscorestr : State -> Element
hscorestr st = 
  let scr = fromString (toString st.hScore)
            |> Text.color purple
            |> Text.height 15
            |> Text.bold
  in container 1200 50 middle (centered scr)

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

pauseText : State -> Text
pauseText st = 
  let message =
      if st.pause
         then if st.ingame || st.score == 0.0
                 then "PRESS SPACE TO START"
                 else "GAME OVER!\nPRESS SPACE TO START"
         else ""
  in fromString message
            |> Text.color black
            |> Text.bold
            |> Text.height 45


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
          `List.append` [toForm (centered (pauseText st))]

ss : State -> String
ss st = toString st.score

--------------------------------
-- Car 
--------------------------------

car : Form
car = toForm <| image 50 75 "sprites/car_number_one.png"

boulderone : Form
boulderone = toForm <| image 100 100 "sprites/rock_one.png"

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

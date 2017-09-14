import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Time exposing (Time, millisecond)
import Array exposing (Array)
import Char exposing (toCode, fromCode)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init = (initModel, Cmd.none)

type alias Model =
  { input: String
  , interval: String
  , time: Int
  , befunge: Befunge
  }

type alias Befunge =
  { source: Array2d Char
  , cursor: (Int, Int)
  , direction: Direction
  , running: Bool
  , mode: Mode
  , stack: Stack
  , output: String
  }

type alias Stack = List Int
type Mode = StringMode | End | None
type Direction = Up | Down | Left | Right
type alias Array2d a = Array (Array a)

pop : Stack -> (Stack, Int)
pop s =
  let
    newStack = s
      |> List.tail
      |> Maybe.withDefault []
    value = s
      |> List.head
      |> Maybe.withDefault 0
  in
    (newStack, value) 

push : Stack -> Int -> Stack
push s v = v :: s

show : Stack -> String
show s = s
  |> List.reverse
  |> List.map toString
  |> String.join " "

emptyArray2d : Array2d a
emptyArray2d = Array.initialize 1 (always Array.empty)

defaultInterval : Float
defaultInterval = 200

defaultInput : String
defaultInput = """2>:1->1-00p::00g:       v
         v%-g00::_v#!`\\-_$$.v
     ^g00_                  v
 ^+1                        <
                  >       :.^"""

initModel : Model
initModel = Model defaultInput (toString defaultInterval) 0 (Befunge (stringToArray defaultInput) (0, 0) Right False End [] "")

type Msg =
  Input String
  | Interval String
  | Toggle
  | Step
  | Reset
  | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input input ->
      let
        b = model.befunge
        befunge = { b |
          source = stringToArray input
        }
      in
        ({ model | 
          input = input,
          befunge = befunge
        }, Cmd.none)
    
    Interval interval ->
      ({ model | interval = interval }, Cmd.none)

    Toggle ->
      let
        b = model.befunge
        befunge = case b.mode of
          End -> initBefunge model
          _ ->
            { b |
              running = not b.running
            }
      in
        ({ model |
          befunge = befunge,
          time = if model.befunge.mode == End then 0 else model.time
        }, Cmd.none)
    
    Reset ->
      let
        b = model.befunge
        befunge = { b |
          cursor = (0, 0),
          direction = Right,
          stack = [],
          output = "",
          running = False,
          source = stringToArray model.input,
          mode = End
        }
      in
        ({ model |
          befunge = befunge,
          time = 0
        }, Cmd.none)

    Tick _ ->
      if model.befunge.running
      then
        let
          frame = model.interval
            |> String.toFloat
            |> Result.withDefault defaultInterval
            |> clamp 0.0001 1.0
            |> (/) 1.0
            |> round
          befunge = List.range 1 frame
            |> List.foldr (always process) model.befunge 
        in
          ({ model |
            time = model.time + frame,
            befunge = befunge
          }, Cmd.none)
      else
        (model, Cmd.none)
    
    Step ->
      let
        befunge = case model.befunge.mode of
          End -> initBefunge model
          _ -> model.befunge
      in
        ({ model |
          time = model.time + 1,
          befunge = process { befunge |
            running = False
          }
        }, Cmd.none)

initBefunge : Model -> Befunge
initBefunge model = 
  let
    b = model.befunge
  in
    { b |
      cursor = (-1, 0),
      direction = Right,
      stack = [],
      output = "",
      running = True,
      source = stringToArray model.input,
      mode = None
    }

stringToArray : String -> Array2d Char
stringToArray source = source
  |> String.split "\n"
  |> List.map String.toList
  |> List.map Array.fromList
  |> Array.fromList

subscriptions : Model -> Sub Msg
subscriptions model = 
  let
    ms = model.interval
      |> String.toFloat
      |> Result.withDefault defaultInterval
      |> clamp 0 5000
  in
    Time.every (millisecond * ms) Tick

cyclicIndex : Array a -> Int -> Maybe Int
cyclicIndex a i = 
  let
    l = Array.length a
  in
    if Array.length a == 0 then Nothing else Just (i % l)

cyclicIndex2d : Array2d a -> (Int, Int) -> Maybe (Int, Int)
cyclicIndex2d a (x, y) =
  let
      cy = cyclicIndex a y
      cx = cy
        |> Maybe.andThen (flip Array.get a)
        |> Maybe.andThen (flip cyclicIndex x)
  in
    Maybe.map2 (,) cx cy
      
get2d : Array2d a -> (Int, Int) -> Maybe a
get2d a (x, y) = a
  |> Array.get y
  |> Maybe.andThen (Array.get x)

set2d : (Int, Int) -> a -> Array2d a -> Array2d a
set2d (x, y) v a = Array.set y (Array.set x v
  <| Maybe.withDefault Array.empty 
  <| Array.get y a) a

indexedMap2d : ((Int, Int) -> a -> b) -> Array2d a -> Array2d b
indexedMap2d f a = a
  |> Array.indexedMap (\y -> Array.indexedMap (\x c -> f (x, y) c))

walkNext : Array2d a -> Direction -> (Int, Int) -> (Int, Int)
walkNext a direction (x, y) =
  let
    cursorCandidate = case direction of
      Left  -> (x - 1, y)
      Right -> (x + 1, y)
      Up    -> (x, y - 1)
      Down  -> (x, y + 1)
  in
    cyclicIndex2d a cursorCandidate
      |> Maybe.withDefault (0, 0)

calc : (Int -> Int -> Int) -> Stack -> Stack
calc f s =
  let
    (s1, y) = pop s
    (s2, x) = pop s1
  in
    push s2 (f x y)

process : Befunge -> Befunge
process b =
  let
    cursor = walkNext b.source b.direction b.cursor
    cell = get2d b.source cursor
      |> Maybe.withDefault ' '
  in
    if b.mode == StringMode && cell /= '"'
      then
        { b |
          stack = push b.stack (toCode cell),
          cursor = cursor
        }
      else
        commands cell cursor { b | cursor = cursor }

commands : Char -> (Int, Int) -> Befunge -> Befunge
commands cell cursor b = case cell of
  '<' -> { b | direction = Left }
  '>' -> { b | direction = Right }
  '^' -> { b | direction = Up }
  'v' -> { b | direction = Down }
  ' ' -> b
  '_' ->
    let
      (s, v) = pop b.stack
    in
      { b |
        stack = s,
        direction = if v == 0 then Right else Left
      }
  '|' ->
    let
      (s, v) = pop b.stack
    in
      { b |
        stack = s,
        direction = if v == 0 then Right else Left
      }
  -- '?' ->
  -- ' ' ->
  '#' -> { b | cursor = walkNext b.source b.direction cursor }
  '@' ->
    { b |
      running = False,
      mode = End
    }
  '0' -> { b | stack = push b.stack 0 }
  '1' -> { b | stack = push b.stack 1 }
  '2' -> { b | stack = push b.stack 2 }
  '3' -> { b | stack = push b.stack 3 }
  '4' -> { b | stack = push b.stack 4 }
  '5' -> { b | stack = push b.stack 5 }
  '6' -> { b | stack = push b.stack 6 }
  '7' -> { b | stack = push b.stack 7 }
  '8' -> { b | stack = push b.stack 8 }
  '9' -> { b | stack = push b.stack 9 }
  '"' ->
    { b |
      mode = if b.mode == StringMode then None else StringMode
    }
  '.' ->
    let
      (s, v) = pop b.stack
    in
      { b |
        stack = s,
        output = b.output ++ (toString v) ++ " "
      }
  ',' ->
    let
      (s, v) = pop b.stack
    in
      { b |
        stack = s,
        output = b.output ++ (v
          |> fromCode
          |> String.fromChar
        )
      }
  '+' -> { b | stack = calc (+) b.stack }
  '-' -> { b | stack = calc (-) b.stack }
  '*' -> { b | stack = calc (*) b.stack }
  '/' -> { b | stack = calc (//) b.stack }
  '%' -> { b | stack = calc (%) b.stack }
  '`' -> { b | stack = calc (\x y -> if x > y then 1 else 0) b.stack }
  '!' ->
    let
      (s, v) = pop b.stack
    in
      { b |
        stack = push s (if v == 0 then 1 else 0)
      } 
  ':' ->
    let
      (s1, v) = pop b.stack
      s2 = push s1 v
    in
      { b |
        stack = push s2 v
      }
  '\\' ->
    let
       (s1, y) = pop b.stack
       (s2, x) = pop s1
       s3 = push s2 y
    in
      { b |
        stack = push s3 x 
      }
  '$' ->
    let
      (s, _) = pop b.stack
    in
      { b |
        stack = s
      }
  'g' ->
    let
      (s1, y) = pop b.stack
      (s2, x) = pop s1
      c = get2d b.source (x, y)
        |> Maybe.map toCode
        |> Maybe.withDefault 0
    in
      { b |
        stack = push s2 c
      }
  'p' ->
    let
      (s1, y) = pop b.stack
      (s2, x) = pop s1
      (s3, v) = pop s2
    in
      { b |
        stack = s3,
        source = set2d (x, y) (fromCode v) b.source
      }
  _ -> b

view : Model -> Html Msg
view model =
  div [ bodyStyle ]
    [ h1 [ titleStyle ] [ text "Befunge" ]
    , div [] [
        textarea [ textStyle, onInput Input, value model.input, rows 10, cols 80 ] []
      ]
    , input [ textStyle, type_ "text", onInput Interval, value model.interval  ] []
    , input [ buttonStyle, type_ "button", onClick Toggle, value (if model.befunge.running then "stop" else "run") ] []
    , input [ buttonStyle, type_ "button", onClick Step, value "step" ] []
    , input [ buttonStyle, type_ "button", onClick Reset, value "reset" ] []
    , div [] [
        div [ textStyle ] [ colorize model.befunge.source model.befunge.cursor ]
      ]
    , div [] [
        div [ textStyle ] [ text (show model.befunge.stack) ]
      ]
    , div [] [
        div [ textStyle ] [ text model.befunge.output ]
      ]
    , div [] [ text (toString model.time) ]
    , div [] [
        a [ footerStyle, href "https://github.com/pnlybubbles/elm-befunge", target "_blank" ] [ text "source" ]
      ]
    ]

fixCharWidth : Char -> Char
fixCharWidth x =
  let
    ac = toCode x 
  in
    if 33 <= ac && ac <= 126
      then x
      else fromCode 160

colorize : Array2d Char -> (Int, Int) -> Html Msg
colorize source (cx, cy) =
  let
    wrap (x, y) cell =
      span (
        if x == cx && y == cy
          then [ cursorStyle ]
          else []
      ) [ text <| String.fromChar <| fixCharWidth cell ]
    children = source 
      |> indexedMap2d wrap
      |> Array.map (\child -> div [] (Array.toList child))
      |> Array.toList
  in
    div [] children

cursorStyle : Attribute Msg
cursorStyle = style [
    ("background-color", "#000"),
    ("color", "#fff")
  ]

titleStyle : Attribute Msg
titleStyle = style [
    ("font-family", "'Arial Black', 'Arial Bold', Gadget, sans-serif")
  ]

bodyStyle : Attribute Msg
bodyStyle = style [
    ("margin", "30px"),
    ("font-family", "Avenir, sans-serif")
  ]

buttonStyle : Attribute msg
buttonStyle = style [
    ("margin-left", "15px")
  ]

textStyle : Attribute Msg
textStyle = style [
    ("font-family", "Monaco, Consolas, 'Courier New', Courier, monospace"),
    ("border", "solid 2px #000"),
    ("padding", "11px 12px 10px"),
    ("display", "inline-block"),
    ("margin", "0 0 15px 0"),
    ("box-sizing", "border-box")
  ]

footerStyle : Attribute Msg
footerStyle = style [
    ("color", "#999"),
    ("font-size", "12px")
  ]

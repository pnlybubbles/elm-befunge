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
init = (model, Cmd.none)

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

model : Model
model = Model "" "500" 0 (Befunge emptyArray2d (0, 0) Right False None [] "")

type Msg =
  Input String
  | Interval String
  | Toggle
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
        befunge = { b |
          cursor = (-1, 0),
          direction = Right,
          stack = [],
          output = "",
          running = not b.running,
          source = stringToArray model.input 
        }
      in
        ({ model | befunge = befunge }, Cmd.none)

    Tick _ ->
      if model.befunge.running
      then
        ({ model |
          time = model.time + 1,
          befunge = process model.befunge
        }, Cmd.none)
      else
        (model, Cmd.none)

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
      |> String.toInt
      |> Result.withDefault 500
      |> toFloat
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
        output = b.output ++ (toString v)
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

calc : (Int -> Int -> Int) -> Stack -> Stack
calc f s =
  let
    (s1, y) = pop s
    (s2, x) = pop s1
  in
    push s2 (f x y)

view : Model -> Html Msg
view model =
  div [ bodyStyle ]
    [ div [] [ textarea [ onInput Input, value model.input, rows 18, cols 100 ] [] ]
    , input [ type_ "text", onInput Interval, value model.interval  ] []
    , input [ type_ "button", onClick Toggle, value (if model.befunge.running then "stop" else "run") ] []
    , div [] [ div [ textStyle ] [ colorize model.befunge.source model.befunge.cursor ] ]
    , div [] [ div [ textStyle ] [ text (show model.befunge.stack) ] ]
    , div [] [ div [ textStyle ] [ text model.befunge.output ] ]
    , div [] [ text (toString model.time) ]
    ]

fixCharWidth : Char -> Char
fixCharWidth x =
  let
    ac = toCode x 
  in
    if 33 <= ac && ac <= 126
      then x
      else fromCode 160

      

bodyStyle : Attribute Msg
bodyStyle = style [
    ("margin", "30px")
  ]

textStyle : Attribute Msg
textStyle = style [
    ("font-family", "Monaco"),
    ("border", "solid 1px #666"),
    ("padding", "11px 12px 10px"),
    ("display", "inline-block"),
    ("margin", "10px 0 0 0"),
    ("box-sizing", "border-box")
  ]

colorize : Array2d Char -> (Int, Int) -> Html Msg
colorize source (cx, cy) =
  let
    wrap (x, y) cell =
      span (
        if x == cx && y == cy
          then [ style [("background-color", "#faa")] ]
          else []
      ) [ text <| String.fromChar <| fixCharWidth cell ]
    children = source 
      |> indexedMap2d wrap
      |> Array.map (\child -> div [] (Array.toList child))
      |> Array.toList
  in
    div [] children

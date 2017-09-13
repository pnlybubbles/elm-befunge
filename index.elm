import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Time exposing (Time, millisecond)
import Array exposing (Array)

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
  , running: Bool
  , time: Int
  , befunge: Befunge
  }

type alias Befunge =
  { source: Array2d Char
  , cursor: (Int, Int)
  , direction: Direction
  , mode: Mode
  , stack: Stack
  , output: String
  }

type alias Stack = List Int
type Mode = StringMode | None
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
model = Model "" "500" False 0 (Befunge emptyArray2d (0, 0) Right None [] "")

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
          source = input
            |> String.split "\n"
            |> List.map String.toList
            |> List.map Array.fromList
            |> Array.fromList
        }
      in
        ({ model | 
          input = input,
          befunge = befunge
        }, Cmd.none)
    
    Interval interval ->
      ({ model | interval = interval }, Cmd.none)

    Toggle ->
      ({ model | running = not model.running }, Cmd.none)

    Tick _ ->
      if model.running
      then
        ({ model |
          time = model.time + 1,
          befunge = process model.befunge
        }, Cmd.none)
      else
        (model, Cmd.none)

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

indexedMap2d : ((Int, Int) -> a -> b) -> Array2d a -> Array2d b
indexedMap2d f a = a
  |> Array.indexedMap (\y -> Array.indexedMap (\x c -> f (x, y) c))

walkNext : Array2d a -> Direction -> (Int, Int) -> (Int, Int)
walkNext a direction (x, y) =
  let
    cursorCandidate = case direction of
      Left  -> (x + 1, y)
      Right -> (x - 1, y)
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
    case cell of
      '<' ->
        { b |
          direction = Left,
          cursor = cursor
        }
      '>' ->
        { b |
          direction = Right,
          cursor = cursor
        }
      '^' ->
        { b |
          direction = Up,
          cursor = cursor
        }
      'v' ->
        { b |
          direction = Down,
          cursor = cursor
        }
      ' ' ->
        { b |
          cursor = cursor
        }
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
      _ ->
        { b |
          cursor = cursor
        }

view : Model -> Html Msg
view model =
  div []
    [ textarea [ onInput Input, value model.input ] []
    , input [ type_ "text", onInput Interval, value model.interval  ] []
    , input [ type_ "button", onClick Toggle ] []
    , div [] [ text model.input ]
    , div [] [ colorize model.befunge.source model.befunge.cursor ]
    , div [] [ text (show model.befunge.stack) ]
    , div [] [ text model.interval ]
    , div [] [ text (toString model.running) ]
    , div [] [ text (toString model.time) ]
    ]

colorize : Array2d Char -> (Int, Int) -> Html Msg
colorize source (cx, cy) =
  let
    wrap (x, y) cell =
      span (
        if x == cx && y == cy
          then [ style [("background-color", "#faa")] ]
          else []
      ) [ text (String.fromChar cell) ]
    children = source 
      |> indexedMap2d wrap
      |> Array.map (\child -> div [] (Array.toList child))
      |> Array.toList
  in
    div [] children

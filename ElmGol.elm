module ElmGol exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Array exposing (..)
import Html.App as App
import Maybe exposing (withDefault)
import List exposing (foldr, sortWith, length)
import Svg exposing (svg, rect)
import Time exposing (Time,second,now)
import Svg.Attributes exposing (width, height,viewBox, fill, x, y, class)
import String exposing (toInt, padLeft, length)
import Random exposing (int, initialSeed, step, list)
import Json.Encode exposing (string)
import Task exposing (perform)


main = 
  App.program { init   = (init 10, setSeed)  
              , update = update
              , view   = view
              , subscriptions = subscriptions
              }

setSeed = perform (\_ -> SetSeed Nothing)
                  (\t -> SetSeed (Just (round t)))
                  now

type Cell = Alive | Dead

type alias Model = 
  { running : Bool
  , generation : Int
  , grid : Array Cell
  , gridSize : Int
  , buffer : String 
  , density : String
  , initSeed : Maybe Int
  , error : Maybe String 
  }

init gs = Model False 0 (repeat (gs * gs) Dead) gs "" "0" Nothing Nothing

-- UPDATE 

type Msg = 
    Step
  | StoreSize String
  | StoreDens String
  | Tick Time
  | Play
  | Pause
  | Reset
  | Resize
  | Click Int
  | SetSeed (Maybe Int)
  | Random 

update : Msg -> Model -> (Model,Cmd Msg)
update msg model =
  case msg of 
    Step ->
      { model 
      | generation = (.generation model) + 1
      , grid = nextStep (.grid model) (.gridSize model) 
      } ! []
    StoreSize s -> 
      {model | buffer = s} ! []

    StoreDens s -> 
      {model | density = s} ! []

    Tick _ -> if (.running model)
              then 
                if countNbAlive (.grid model) == 0
                then {model | running = False} ! []
                else  
                  { model 
                  | generation = (.generation model) + 1
                   , grid = nextStep (.grid model) (.gridSize model) 
                  } ! []
              else model ! []
    Play -> 
      { model
      | running = True 
      } ! []
    Pause -> 
      { model
      | running = False 
      } ! []
    Reset -> 
      { model
      | running = False
      , generation = 0
      , grid = (repeat (.gridSize model * .gridSize model) Dead)
      } ! []
    Resize ->
      case toInt (.buffer model) of 
        Err _ -> model ! []
        Ok n -> 
          { model
          | running = False
          , gridSize = n
          , generation = 0
          , grid = (repeat (n * n) Dead)
          } ! [] 
    Click n -> 
      { model
      | grid = 
          if (.running model)
          then (.grid model)
          else swap n (.grid model)  
      } ! []
    SetSeed n ->
      { model | initSeed = n } ! []
    Random  ->
      case .initSeed model of
        Nothing -> {model | error = Just "No random seed"} ! []
        Just sd ->
          case toInt (.density model) of 
            Err s -> {model | error = Just s} ! []
            Ok n  -> 
              { model
              | running = False
              , generation = 0
              , grid = randomize (.grid model) (.gridSize model) sd n
              } ! [setSeed]


swap : Int -> Array Cell -> Array Cell
swap n grid =
  case get n grid of 
    Nothing -> grid
    Just s  ->
      case s of 
        Dead -> set n Alive grid
        Alive -> set n Dead grid 

nextStep : Array Cell -> Int -> Array Cell
nextStep grid gs = 
  let nextCell n cell = 
        let nb = countNeighbours n
            in
            case cell of 
              Alive -> 
                if (nb  == 2 || nb == 3)
                then Alive
                else Dead
              Dead -> 
                if nb == 3 
                then Alive
                else Dead 
            

      countNeighbours n = 
        let i  = n // gs
            j  = n %  gs
            upLeft =              
             withDefault Dead (get (((i-1) % gs) * gs + ((j-1) % gs)) grid)
            up =              
             withDefault Dead (get (((i-1) % gs) * gs + (j % gs)) grid)
            upRight =              
             withDefault Dead (get (((i-1) % gs) * gs + ((j+1) % gs)) grid)
            left =              
             withDefault Dead (get ((i % gs) * gs + ((j-1) % gs)) grid)
            right =              
             withDefault Dead (get ((i % gs) * gs + ((j+1) % gs)) grid)
            downLeft =              
             withDefault Dead (get (((i+1) % gs) * gs + ((j-1) % gs)) grid)
            down =              
             withDefault Dead (get (((i+1) % gs) * gs + (j % gs)) grid)
            downRight =              
             withDefault Dead (get (((i+1) % gs) * gs + ((j+1) % gs)) grid)
            
            count = List.foldr
                          (\c acc -> if c == Alive then acc + 1 else acc)
                           0
                           [upLeft,up,upRight,left
                           ,right,downLeft,down,downRight
                           ]
        in count 

  in indexedMap nextCell grid 

countNbAlive : Array Cell -> Int
countNbAlive = 
  Array.foldr 
         (\c acc -> if c == Alive then acc + 1 else acc)
         0 

-- VIEW 
view : Model -> Html Msg
view model =
  let sizeCoordSys = toString ((.gridSize model) * 20)
      sizeViewPort = toString ((.gridSize model) * 30) ++ "px"
  in 
  div [id "main"]
      [ h1 [] [Html.text "The Game of Life"]
      , text "grid size: "
      , input [ type' "form"
              , placeholder (toString (.gridSize model))
              , style [ ("width","30px")
                      ]
              , onInput StoreSize
              ]
              []
      , button [ type' "button"
               , onClick Resize
               ]
               [ text "Resize"]
      , br [] []
      , br [] []
      , text "density: "
      , input [ type' "range"
              , Html.Attributes.min "0"
              , Html.Attributes.max "100"
              , Html.Attributes.value (.density model)
              , onInput StoreDens
              , style [ ("width","100px")
                      , ("vertical-align","bottom")
                      , ("outline","none")
                      ]
              ] []
      , span [style [("font-family","Consolas,Monaco,Lucida Console,Liberation Mono
                       ,DejaVu Sans Mono,Bitstream Vera Sans Mono
                       ,Courier New, monospace")
                    ,("white-space","pre")
                    ]
             ] 
             [text <| (padLeft 3 ' ' (.density model)) ++ "% "]
      --, br [] []
      , button [ type' "button"
               , onClick Random
               ]
               [ text "Randomize"]
      , br [] []
      , br [] []
      , Svg.svg
          [ viewBox ("0 0 "  ++ sizeCoordSys ++ " " ++ sizeCoordSys)
          , Svg.Attributes.width "400px"
          ]
          ((renderGrid model) ++
          [ 
          ]
          )
      , br [] []
      , button [onClick Play]
               [text "Play"]
      , button [onClick Pause]
               [text "Pause"]
      , button [onClick Reset]
               [text "Reset"]
      , button [onClick Step]
               [text "next step"]
      , br [] []
      , text ("Géneration n° " ++ (toString (.generation model)))     
      , case (.error model) of 
         Nothing -> text ""
         Just e  -> p [] [text <| "Error: " ++ e]
      , style' ".cell:hover {fill: #8FBC8F; } input[type=range]{
                border: 1px solid white;
                outline: 2px solid white;
                outline-offset: -1px;
                }
                input[type='range']::-moz-focus-outer { border: 0; }"
      ]

renderGrid : Model -> List (Html Msg)
renderGrid model = 
  let gs = .gridSize model

      renderCell n cell =
        let i   = (n // gs * 20)
            j   = (n %  gs * 20)
            col = case cell of 
                    Dead  -> "grey"
                    Alive -> "black"

        in rect [ x (toString j)
                , y (toString i)
                , Svg.Attributes.width "20"
                , Svg.Attributes.height "21"
                , fill col
                , onClick (Click n)
                , Svg.Attributes.class "cell"
                ]
                []
  in toList (indexedMap renderCell (.grid model))



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = 
  Time.every (0.1*second) Tick

-- CSS
style' : String -> Html msg
style' text =
    Html.node "style"
        [ property "textContent" <| string text
        , property "type" <| string "text/css"
        ]
        []

-- Utils
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) ->
        (x,y) :: zip xs' ys'

    (_, _) ->
        []

shuffle : List a -> Int -> List a 
shuffle xs seed = 
  let l = List.length xs
      g = int 0 (10*l)
      indexesGen = Random.list l g
      indexList = fst (Random.step indexesGen (initialSeed seed))
      sortedList = sortWith (\ (i1,_) (i2,_) -> compare i1 i2) (zip indexList xs)
  in List.map snd sortedList

makeRange : Int -> List Int
makeRange n = 
  let helper acc n = 
        if n == 0
        then (n :: acc) 
        else helper (n::acc) (n-1)
  in helper [] (n-1)  

randomize : Array Cell -> Int -> Int -> Int -> Array Cell
randomize grid gs sd n = 
  let indexes = makeRange (gs*gs)
      scaledN  = round <| (toFloat <| gs*gs) * (toFloat n/100)
      randIndexes = List.take scaledN <| shuffle indexes sd
      newGrid = initialize (gs*gs) (always Dead)
  in List.foldr (\p acc -> set p Alive acc) newGrid randIndexes 
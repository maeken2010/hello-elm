module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Set exposing (Set, fromList, member, union)
import List exposing (range)


debugWithToString : a -> a
debugWithToString a = Debug.log (Debug.toString a) a

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

type Msg = ToDown | ToRight | ToLeft | RotaionRight

-- model
type Direction = Right | Down | Left
type alias Coordinate = (Int, Int) -- (x, y). xは横, yは縦. 左上が(1, 1)
type alias CellSet = Set Coordinate
type MinoD = Noth | East | South | West
type MinoK = T
type alias Mino = { point : Coordinate, minoD : MinoD, minoK : MinoK }
type alias Model = { mino : Mino, block : CellSet }
init : Model
init = initBoard

mino : Mino -> CellSet
mino m =
    case m.minoK of
        T -> minoT m.minoD m.point

minoT : MinoD -> Coordinate -> CellSet
minoT d (x, y) =
    case d of
        Noth -> fromList [(x, y - 1), (x - 1, y), (x, y), (x + 1, y)]
        East -> fromList [(x, y - 1), (x, y), (x + 1, y), (x, y + 1)]
        South -> fromList [(x - 1, y), (x, y), (x + 1, y), (x, y + 1)]
        West -> fromList [(x, y - 1), (x - 1, y), (x, y), (x, y + 1)]

boardSizeX : Int
boardSizeX = 10

boardSizeY : Int
boardSizeY = 20

bottomWall : CellSet
bottomWall = List.range 1 boardSizeX
    |> List.map (\x -> (x, boardSizeY + 1))
    |> Set.fromList

rightWall : CellSet
rightWall = List.range 1 boardSizeY
    |> List.map (\y -> (11, y))
    |> Set.fromList

leftWall : CellSet
leftWall = List.range 1 boardSizeY
    |> List.map (\y -> (0, y))
    |> Set.fromList

wall : CellSet
wall = Set.union bottomWall rightWall |> Set.union leftWall

initMino : Mino
initMino = {point = (2, 2), minoD = Noth, minoK = T}

initBoard : Model
initBoard = {mino = initMino, block = wall}

-- update
update : Msg -> Model -> Model
update msg model = case msg of
    ToDown -> moveMinoDown model
    ToRight -> moveMinoHorizon model Right
    ToLeft -> moveMinoHorizon model Left
    RotaionRight -> rotationRightMino model

moveMinoDown : Model -> Model
moveMinoDown model = 
    let m = nextMino model.mino Down
    in if canMoveMino {model | mino = m}
        then {model | mino = m}
        else {block = union model.block (mino model.mino), mino = initMino}


moveMinoHorizon : Model -> Direction -> Model
moveMinoHorizon model direction = 
    let m = nextMino model.mino direction
    in if canMoveMino {model | mino = m}
        then {model | mino = m}
        else model

rotationRightRule : MinoD -> MinoD
rotationRightRule d = case d of
    Noth -> East
    East -> South
    South -> West
    West -> Noth

rotationRightMino : Model -> Model
rotationRightMino model =
    let d = rotationRightRule model.mino.minoD
        m = model.mino
        nm = {m | minoD = d}
    in if canMoveMino {model | mino = nm}
        then {model | mino = nm}
        else model


nextMino : Mino -> Direction -> Mino
nextMino m direction =
    let x = Tuple.first m.point
        y = Tuple.second m.point
    in case direction of
            Right -> {m | point = (x + 1, y)}
            Down  -> {m | point = (x, y + 1)}
            Left  -> {m | point = (x - 1, y)}

canMoveMino : Model -> Bool
canMoveMino model =
    mino model.mino
    |> Set.toList
    |> List.filter (\a -> member a model.block)
    |> List.isEmpty

-- view
cellSize : String
cellSize = "15px"

view : Model -> Html Msg
view model = div [] [
        div [] [
         viewBoard model,
         viewController
        ]
    ]

viewBoard : Model -> Html Msg
viewBoard model = div [
        style "display" "flex",
        style "border" "solid",
        style "width" "150px"
    ] (List.map (\x -> viewCellColumn (union model.block (mino model.mino)) x) (range 1 boardSizeX))

viewController : Html Msg
viewController = 
    div [] [
        button [onClick ToLeft] [text "←"],
        button [onClick ToDown] [text "↓"],
        button [onClick ToRight] [text "→"],
        button [onClick RotaionRight] [text "右回り"]
    ]

viewCellColumn : CellSet -> Int -> Html Msg
viewCellColumn cellSet x = div [
        style "display" "flex",
        style "flex-direction" "column"
    ] (
        List.map (\y -> cellDiv cellSet (x, y)) (range 1 boardSizeY)
    )

cellDiv : CellSet -> Coordinate -> Html Msg
cellDiv cellSet coordinate = div (cellAttribute cellSet coordinate) []

cellAttribute : CellSet -> Coordinate -> List (Attribute Msg)
cellAttribute cellSet coordinate = [
        style "background-color" (cellColor cellSet coordinate),
        style "height" cellSize,
        style "width" cellSize
    ]

cellColor : CellSet -> Coordinate -> String
cellColor cellSet coordinate = if member coordinate cellSet then "black" else "white"


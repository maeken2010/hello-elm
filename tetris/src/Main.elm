module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Set exposing (Set, fromList, member, empty, union, map)
import List exposing (map, range)
import String exposing (right)

debugWithToString : a -> a
debugWithToString a = Debug.log (Debug.toString a) a

boardSizeX : Int
boardSizeX = 10

boardSizeY : Int
boardSizeY = 20

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

type Msg = ToDown | ToRight | ToLeft

-- model
type Direction = Right | Down | Left
type alias Coordinate = (Int, Int)
type alias CellSet = Set Coordinate
type alias Mino = Set Coordinate

type alias Model = { mino : Mino, block : CellSet }
init : Model
init = initBoard

minoT : CellSet
minoT = fromList [(1,1), (2,1), (3,1), (2,2)]

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

initBoard : Model
initBoard = {mino = minoT, block = wall}

-- update
update : Msg -> Model -> Model
update msg model = case msg of
    ToDown -> moveMinoDown model
    ToRight -> moveMinoHorizon model Right
    ToLeft -> moveMinoHorizon model Left

moveMinoDown : Model -> Model
moveMinoDown model = 
    let mino = nextMino model.mino Down
    in if canMoveMino model.block mino
        then {model | mino = mino}
        else {block = union model.block model.mino, mino = minoT}


moveMinoHorizon : Model -> Direction -> Model
moveMinoHorizon model direction = 
    let mino = nextMino model.mino direction
    in if canMoveMino model.block mino
        then {model | mino = mino}
        else model

nextMino : Mino -> Direction -> Mino
nextMino mino direction = case direction of
   Right -> Set.map (\(x, y) -> (x + 1, y)) mino
   Down  -> Set.map (\(x, y) -> (x, y + 1)) mino
   Left -> Set.map (\(x, y) -> (x - 1, y)) mino

canMoveMino : CellSet -> Mino -> Bool
canMoveMino block mino =
    mino
    |> Set.toList
    |> List.filter (\a -> member a block)
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
    ] (List.map (\x -> viewCellColumn (union model.block model.mino) x) (range 1 boardSizeX))

viewController : Html Msg
viewController = 
    div [] [
        button [onClick ToLeft] [text "←"],
        button [onClick ToDown] [text "↓"],
        button [onClick ToRight] [text "→"]
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


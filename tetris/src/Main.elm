module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Set exposing (Set, fromList, member, empty, union, map)
import List exposing (map, range)
import Html exposing (a)

debugWithToString : a -> a
debugWithToString a = Debug.log (Debug.toString a) a

boardSizeX = 10
boardSizeY = 20

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

type Msg = Click

-- model
type alias Coordinate = (Int, Int)
type alias CellSet = Set Coordinate
type alias Mino = Set Coordinate

type alias Model = { mino : Mino, block : CellSet }
init : Model
init = initBoard

minoT : CellSet
minoT = fromList [(1,1), (2,1), (3,1), (2,2)]

bottomBlock : CellSet
bottomBlock = List.range 1 boardSizeX
    |> List.map (\x -> (x, boardSizeY + 1))
    |> Set.fromList

initBoard : Model
initBoard = {mino = minoT, block = bottomBlock}

-- update
update : Msg -> Model -> Model
update msg model = case msg of
    Click -> downMino model

downMino : Model -> Model
downMino model = 
    let nextMino = (Set.map (\(x, y) -> (x, y + 1)) model.mino)
    in if canDownMino model.block nextMino
        then {model | mino = nextMino}
        else {block = union model.block model.mino, mino = minoT}

canDownMino : CellSet -> Mino -> Bool
canDownMino block mino =
    mino
    |> Set.toList
    |> List.filter (\a -> member a block)
    |> List.isEmpty

-- view
cellSize : String
cellSize = "15px"

view : Model -> Html Msg
view model = div [onClick Click] [
        div [
            style "display" "flex",
            style "border" "solid",
            style "width" "150px"
        ] (
            List.map (\x -> viewCellColumn (union model.block model.mino) x) (range 1 boardSizeX)
        )
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


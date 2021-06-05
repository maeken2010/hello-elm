module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Set exposing (Set, fromList, member, empty, union, map)
import List exposing (map, range)

boardSizeX = 10
boardSizeY = 20

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

type Msg = Click

-- model
type alias Coordinate = (Int, Int)
type alias CellSet = Set Coordinate
type alias Model = { mino : CellSet, block : CellSet }
init : Model
init = initBoard

minoT : CellSet
minoT = fromList [(1,1), (2,1), (3,1), (2,2)]

initBoard : Model
initBoard = {mino = minoT, block = empty}

-- update
update : Msg -> Model -> Model
update msg model = case msg of
   Click -> {block = model.block, mino = (Set.map (\(x, y) -> (x + 1, y)) model.mino)}

-- view
cellSize : String
cellSize = "15px"

view : Model -> Html Msg
view model = div [onClick Click] [
        div [style "border" "solid", style "width" "150px"] (List.map (\y -> viewCellRow (union model.block model.mino) y) (range 1 boardSizeY))
    ]

viewCellRow : CellSet -> Int -> Html Msg
viewCellRow cellSet y = div [style "display" "flex"] (List.map (\x -> cellDiv cellSet (y, x)) (range 1 boardSizeX))

cellDiv : CellSet -> Coordinate -> Html Msg
cellDiv cellSet coordinate = div (cellAttribute cellSet coordinate) []

cellAttribute : CellSet -> Coordinate -> List (Attribute Msg)
cellAttribute cellSet coordinate = [style "background-color" (cellColor cellSet coordinate), style "height" cellSize, style "width" cellSize]

cellColor : CellSet -> Coordinate -> String
cellColor cellSet coordinate = if member coordinate cellSet then "black" else "white"


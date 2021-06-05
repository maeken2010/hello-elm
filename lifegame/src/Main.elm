module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import List exposing (map, range, sum, concat, filter)
import Set exposing (Set, insert, member, remove, fromList)

boardSize : Int
boardSize = 50

cellSize : String
cellSize = "15px"

initBoard : Model
initBoard = fromList [(5,5), (6,4), (6,5), (6,6), (11,4), (11,5), (11,6), (12,5)]

main : Program () Model Msg
main = Browser.sandbox { init = init, update = update, view = view }

type alias CellXY = (Int, Int)
type alias Model = Set CellXY

init : Model
init = initBoard

type Msg = ClickCell CellXY | NextGeneration

update : Msg -> Model -> Model
update msg model = case msg of
    ClickCell cellXY -> if member cellXY model then remove cellXY model else insert cellXY model
    NextGeneration -> generate model

view : Model -> Html Msg
view model = div [] [
        nextGenerationButton
        , boardDiv model
    ] 

nextGenerationButton : Html Msg
nextGenerationButton = button [onClick NextGeneration] [text "next"]

boardDiv : Model -> Html Msg
boardDiv model = div [] (map (\row -> viewCellRow model row) (range 1 boardSize))

viewCellRow : Model -> Int -> Html Msg
viewCellRow model row = div [style "display" "flex"] (map (\column -> cellDiv model (row, column)) (range 1 boardSize))

cellDiv : Model -> CellXY -> Html Msg
cellDiv model cellXY = div (cellAttribute model cellXY) []

cellAttribute : Model -> CellXY -> List (Attribute Msg)
cellAttribute model cellXY = [style "background-color" (cellColor model cellXY), style "height" cellSize, style "width" cellSize, onClick (ClickCell cellXY)]

cellColor : Model -> CellXY -> String
cellColor model cellXY = if member cellXY model then "black" else "white"


-- utils
boardCoordinate : List CellXY
boardCoordinate = map (\x -> map (\y -> (x,y)) (range 1 boardSize)) (range 1 boardSize)
    |> concat

generate : Model -> Model
generate model = filter (\x -> (nextSurvive model x)) boardCoordinate
    |> fromList

nextSurvive : Model -> CellXY -> Bool
nextSurvive model cellXY =
    neighborhood cellXY
    |> map (\cell -> if member cell model then 1 else 0)
    |> sum
    |> survivalRule (member cellXY model)

neighborhood : CellXY -> List CellXY
neighborhood (x, y) = [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

survivalRule : Bool -> Int -> Bool
survivalRule live neighborhoodLiveCount =
    if live then
        case neighborhoodLiveCount of
            2 -> True
            3 -> True
            _ -> False
    else
        case neighborhoodLiveCount of
            3 -> True
            _ -> False

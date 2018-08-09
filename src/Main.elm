module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, style, class)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Random.Extra exposing (frequency, constant)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)



---- GENERATORS ----


paintingGenerator : Generator (List Row)
paintingGenerator =
    Random.int 4 6
        |> Random.andThen (\rowCount -> Random.list rowCount rowGenerator)


rowGenerator : Generator Row
rowGenerator =
    Random.int 3 10
        |> Random.andThen (\cellCount -> Random.map2 Row (Random.int 5 15) (Random.list cellCount cellGenerator))


cellGenerator : Generator Cell
cellGenerator =
    Random.map2 Cell (Random.int 10 50) colorGenerator


colorFrequencies : List ( Float, Generator Color )
colorFrequencies =
    [ ( 90, constant White ), ( 3, constant Red ), ( 3, constant Yellow ), ( 3, constant Blue ) ]


colorGenerator : Generator Color
colorGenerator =
    frequency colorFrequencies



---- MODEL ----


type alias Model =
    { painting : Painting }


type alias Painting =
    List Row


type alias Row =
    { height : Int
    , cells : List Cell
    }


type alias Cell =
    { width : Int
    , color : Color
    }


type Color
    = White
    | Red
    | Blue
    | Yellow


initialModel =
    { painting =
        [ { height = 20, cells = [ { width = 30, color = "#fff" } ] }
        , { height = 40, cells = [ { width = 30, color = "#00ff00" } ] }
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( { painting = [] }, newPaintingCmd )


totalHeightForRows : Painting -> Int
totalHeightForRows rows =
    rows
        |> List.map .height
        |> List.sum


totalWidthForCells : List Cell -> Int
totalWidthForCells cells =
    cells
        |> List.map .width
        |> List.sum



---- UPDATE ----


newPaintingCmd =
    Random.generate NewPainting paintingGenerator


type Msg
    = NewPainting Painting
    | GeneratePainting


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPainting p ->
            ( { model | painting = p }, Cmd.none )

        GeneratePainting ->
            ( model, newPaintingCmd )



---- VIEW ----


numberToPx : number -> String
numberToPx i =
    (toString i) ++ "px"


colorToString : Color -> String
colorToString color =
    case color of
        White ->
            "white"

        Red ->
            "red"

        Yellow ->
            "yellow"

        Blue ->
            "blue"


lengthProportion : Int -> Int -> Float
lengthProportion totalHeight height =
    (toFloat height) / (toFloat totalHeight)


renderCell : Int -> Cell -> Html msg
renderCell totalWidth { width, color } =
    let
        actualWidth =
            containerWidth * (lengthProportion totalWidth width)
    in
        div
            [ style
                [ "width" => (numberToPx actualWidth)
                , "background-color" => (colorToString color)
                ]
            , class "cell"
            ]
            []


renderRow : Int -> Row -> Html msg
renderRow totalHeight { height, cells } =
    let
        totalWidth =
            totalWidthForCells cells

        actualHeight =
            containerHeight * (lengthProportion totalHeight height)

        styles =
            [ "height" => (numberToPx actualHeight) ]
    in
        div [ style styles, class "row" ]
            (List.map (renderCell totalWidth) cells)


containerWidth =
    400


containerHeight =
    400


view : Model -> Html Msg
view { painting } =
    let
        styles =
            [ "width" => (numberToPx containerWidth)
            , "height" => (numberToPx containerHeight)
            ]

        totalHeight =
            totalHeightForRows painting
    in
        div []
            [ div [ class "main", style styles ]
                (List.map (renderRow totalHeight) painting)
            , button [ onClick GeneratePainting ] [ text "New painting" ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

module Main exposing (Model, Msg(..), init, main, rawText, update, view)

import Browser
import Html exposing (Html, div, h1, h2, img, text)
import Html.Attributes exposing (src)
import List.Extra
import Parser exposing ((|.), (|=), Parser, spaces, succeed, symbol)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


rawText : String
rawText =
    "    _  _     _  _  _  _  _ \n  | _| _||_||_ |_   ||_||_|\n  ||_  _|  | _||_|  ||_| _|\n\n"


type Bar
    = LitBar
    | EmptyBar


type alias NumberFromBars =
    List Bar


validOne : NumberFromBars
validOne =
    [ EmptyBar, EmptyBar, EmptyBar, EmptyBar, EmptyBar, LitBar, EmptyBar, EmptyBar, LitBar ]


validTwo : NumberFromBars
validTwo =
    [ EmptyBar, LitBar, EmptyBar, LitBar, LitBar, LitBar, LitBar, LitBar, EmptyBar ]


validThree : NumberFromBars
validThree =
    [ EmptyBar, LitBar, EmptyBar, EmptyBar, LitBar, LitBar, EmptyBar, LitBar, LitBar ]



-- TODO: more valid digits, through nine, and don't forget zero


convertFourLinesToRawNums : String -> String
convertFourLinesToRawNums input =
    let
        listOfLines =
            input
                |> String.replace " " "w"
                |> String.split "\n"
                |> List.take 3
                |> List.map breakOneLineIntoLists

        combinedLines =
            case List.Extra.getAt 0 listOfLines of
                Nothing ->
                    "bad string"

                Just stringA ->
                    case List.Extra.getAt 1 listOfLines of
                        Nothing ->
                            "bad string"

                        Just stringB ->
                            case List.Extra.getAt 2 listOfLines of
                                Nothing ->
                                    "bad string"

                                Just stringC ->
                                    combineLines stringA stringB stringC
                                        |> String.join "q"
    in
    combinedLines



-- Function to take ONE string of 27 chars and return a list of nine strings of three chars each


breakOneLineIntoLists : String -> List String
breakOneLineIntoLists inputString =
    inputString
        |> String.toList
        |> List.Extra.groupsOf 3
        |> List.map String.fromList



-- Function to take THREE lists of nine three-character strings as input, and combine them into ONE list of nine-character strings
-- combineLines : List String -> List String -> List String -> List String
-- combineLines a b c =
--     List.concat [ a, b, c ]
--         |> List.Extra.groupsOf 9
--         |> List.concat


combineLines : List String -> List String -> List String -> List String
combineLines a b c =
    let
        line1s =
            case a of
                [] ->
                    []

                x :: xs ->
                    xs

        line2s =
            case b of
                [] ->
                    []

                x :: xs ->
                    xs

        line3s =
            case c of
                [] ->
                    []

                x :: xs ->
                    xs

        line1first =
            case a of
                [] ->
                    []

                x :: xs ->
                    [ x ]

        line2first =
            case b of
                [] ->
                    []

                x :: xs ->
                    [ x ]

        line3first =
            case c of
                [] ->
                    []

                x :: xs ->
                    [ x ]

        -- nextChunks =
        --     combineLines line1s line2s line3s
    in
    line1first ++ line2first ++ line3first


sumOfSquares : Int -> Int
sumOfSquares n =
    case n of
        0 ->
            0

        _ ->
            (n ^ 2) + sumOfSquares (n - 1)


convertLineOfText : String -> List (Maybe NumberFromBars)
convertLineOfText text =
    [ Just validOne ]


thingTest : Parser Bar
thingTest =
    succeed LitBar
        |. symbol " "
        |. symbol "_"
        |. symbol " "
        |. symbol "|"
        |. symbol "_"
        |. symbol "|"
        |. symbol "|"
        |. symbol "_"
        |. symbol "|"



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        someList =
            convertFourLinesToRawNums rawText

        someString =
            someList
    in
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]

        -- , h2 [] [ text (Maybe.withDefault "nothing to see here" someList) ]
        , h2 [] [ text someList ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

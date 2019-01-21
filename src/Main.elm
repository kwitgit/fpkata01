module Main exposing (Model, Msg(..), init, main, rawText, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Parser exposing ((|.), (|=), Parser, spaces, succeed, symbol)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


rawText : String
rawText =
    "       _  _     _  _  _  _  _\n     | _| _||_||_ |_   ||_||_|\n     ||_  _|  | _||_|  ||_| _|\n\n"


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
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
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

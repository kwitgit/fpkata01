module Main exposing
    ( Model
    , Msg(..)
    , init
    , main
    , update
    , view
    )

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Task



---- MODEL ----


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
    [ EmptyBar, LitBar, EmptyBar, EmptyBar, LitBar, LitBar, LitBar, LitBar, EmptyBar ]


validThree : NumberFromBars
validThree =
    [ EmptyBar, LitBar, EmptyBar, EmptyBar, LitBar, LitBar, EmptyBar, LitBar, LitBar ]


validFour : NumberFromBars
validFour =
    [ EmptyBar, EmptyBar, EmptyBar, LitBar, LitBar, LitBar, EmptyBar, EmptyBar, LitBar ]


validFive : NumberFromBars
validFive =
    [ EmptyBar, LitBar, EmptyBar, LitBar, LitBar, EmptyBar, EmptyBar, LitBar, LitBar ]


validSix : NumberFromBars
validSix =
    [ EmptyBar, LitBar, EmptyBar, LitBar, LitBar, EmptyBar, LitBar, LitBar, LitBar ]


validSeven : NumberFromBars
validSeven =
    [ EmptyBar, LitBar, EmptyBar, EmptyBar, EmptyBar, LitBar, EmptyBar, EmptyBar, LitBar ]


validEight : NumberFromBars
validEight =
    [ EmptyBar, LitBar, EmptyBar, LitBar, LitBar, LitBar, LitBar, LitBar, LitBar ]


validNine : NumberFromBars
validNine =
    [ EmptyBar, LitBar, EmptyBar, LitBar, LitBar, LitBar, EmptyBar, LitBar, LitBar ]


validZero : NumberFromBars
validZero =
    [ EmptyBar, LitBar, EmptyBar, LitBar, EmptyBar, LitBar, LitBar, LitBar, LitBar ]


type alias AccountInfo =
    { accountNum : String
    , checksum : CustomChecksum
    }


type alias Model =
    { accountList : List AccountInfo }


rawText : String
rawText =
    "    _  _     _  _  _  _  _ \n  | _| _||_||_ |_   ||_||_|\n  ||_  _|  | _||_|  ||_| _|\n\n"


convertRawNumToNumberFromBars : List String -> NumberFromBars
convertRawNumToNumberFromBars listOfRawNums =
    listOfRawNums
        |> List.map convertRawNumToBar
        |> List.concat


convertRawNumToBar : String -> NumberFromBars
convertRawNumToBar input =
    input
        |> String.toList
        |> List.map
            (\c ->
                if c == '_' then
                    LitBar

                else if c == '|' then
                    LitBar

                else
                    EmptyBar
            )



-- function to take an entire file, given as a string,
-- separate it out into individual groups of four lines to
-- each be turned into an account number


getGroupsOfFourLines : String -> List String
getGroupsOfFourLines stringOfFile =
    stringOfFile
        |> String.split "\n\n"


convertFourLinesToRawNums : String -> List String
convertFourLinesToRawNums input =
    let
        listOfLines =
            input
                |> String.split "\n"
                |> List.take 3
                |> List.map breakOneLineIntoLists

        combinedLines =
            case List.Extra.getAt 0 listOfLines of
                Nothing ->
                    []

                Just stringA ->
                    case List.Extra.getAt 1 listOfLines of
                        Nothing ->
                            []

                        Just stringB ->
                            case List.Extra.getAt 2 listOfLines of
                                Nothing ->
                                    []

                                Just stringC ->
                                    combineLines stringA stringB stringC
    in
    combinedLines



-- Function to take ONE string , which is a line of 27 chars, and return a list of NINE strings of three chars each


breakOneLineIntoLists : String -> List String
breakOneLineIntoLists inputString =
    inputString
        |> String.toList
        |> List.Extra.groupsOf 3
        |> List.map String.fromList



-- Function to take THREE lists of nine three-character strings as input, and combine them into ONE list of nine-character strings


combineLines : List String -> List String -> List String -> List String
combineLines a b c =
    let
        line1s =
            case a of
                [] ->
                    []

                [ x ] ->
                    []

                x :: xs ->
                    xs

        line2s =
            case b of
                [] ->
                    []

                [ x ] ->
                    []

                x :: xs ->
                    xs

        line3s =
            case c of
                [] ->
                    []

                [ x ] ->
                    []

                x :: xs ->
                    xs

        line1first =
            case a of
                [] ->
                    []

                [ x ] ->
                    [ x ]

                x :: xs ->
                    [ x ]

        line2first =
            case b of
                [] ->
                    []

                [ x ] ->
                    [ x ]

                x :: xs ->
                    [ x ]

        line3first =
            case c of
                [] ->
                    []

                [ x ] ->
                    [ x ]

                x :: xs ->
                    [ x ]

        nextChunks =
            if List.length line1s > 0 then
                combineLines line1s line2s line3s

            else
                []
    in
    line1first ++ line2first ++ line3first ++ nextChunks


init : ( Model, Cmd Msg )
init =
    -- let
    ( { accountList = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = OpenFileClicked
    | FileSelected File
    | FileRead String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenFileClicked ->
            ( model, Select.file [] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileRead (File.toString file) )

        FileRead content ->
            let
                listOfAccountGroups : List String
                listOfAccountGroups =
                    getGroupsOfFourLines content

                listOfRawNums : List (List String)
                listOfRawNums =
                    listOfAccountGroups
                        |> List.map convertFourLinesToRawNums

                listOfAccounts : List NumberFromBars
                listOfAccounts =
                    listOfRawNums
                        |> List.map convertRawNumToNumberFromBars

                listOfChecksummedAccounts : List AccountInfo
                listOfChecksummedAccounts =
                    listOfAccounts
                        |> List.map createAccountInfo
            in
            ( { model
                | accountList = listOfChecksummedAccounts
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , div [ class "f4 bold center mw6 courier" ]
            [ button [ onClick OpenFileClicked ] [ text "Pick file" ]
            ]
        , h2 [ class "f4 bold center mw6 courier" ]
            [ ul [ class "list left mw6 ba b--light-silver br2" ]
                (renderAccountList
                    model
                )
            ]
        ]



-- Take the list of account numbers (in the model), get valid numbers for
-- all accounts in the list,


renderAccountList : Model -> List (Html msg)
renderAccountList model =
    let
        listOfAccounts =
            model.accountList

        -- collapsedNums =
        --     listOfAccounts
        --         |> List.map getAccountNum
    in
    List.map (\l -> li [] [ renderSingleAccount l ]) listOfAccounts


renderSingleAccount : AccountInfo -> Html msg
renderSingleAccount accountInfo =
    let
        checksumString =
            case accountInfo.checksum of
                ChecksumError ->
                    "Illegible!"

                ChecksumAmbiguous stringList ->
                    String.concat [ "Ambiguous! ", String.concat stringList ]

                CalcedChecksum checkSum ->
                    if checkSum == 0 then
                        "Good!"

                    else
                        "Bad checksum: " ++ String.fromInt checkSum
    in
    text (String.concat [ accountInfo.accountNum, ", ", checksumString ])


getValidNums : NumberFromBars -> List String
getValidNums input =
    let
        output =
            input
                |> List.Extra.groupsOf 9

        output2 =
            output
                |> List.map
                    (\n ->
                        if n == validOne then
                            "1"

                        else if n == validTwo then
                            "2"

                        else if n == validThree then
                            "3"

                        else if n == validFour then
                            "4"

                        else if n == validFive then
                            "5"

                        else if n == validSix then
                            "6"

                        else if n == validSeven then
                            "7"

                        else if n == validEight then
                            "8"

                        else if n == validNine then
                            "9"

                        else if n == validZero then
                            "0"

                        else
                            "?"
                     -- Debug.toString n
                    )
    in
    output2


getAccountNum : NumberFromBars -> String
getAccountNum input =
    input
        |> getValidNums
        |> String.concat


createAccountInfo : NumberFromBars -> AccountInfo
createAccountInfo input =
    let
        accountNum =
            getAccountNum input

        thisChecksum =
            calcChecksum accountNum
    in
    { accountNum = accountNum
    , checksum = thisChecksum
    }



-- account number:  3  4  5  8  8  2  8  6  5
-- position names:  d9 d8 d7 d6 d5 d4 d3 d2 d1
-- checksum calculation:
-- (d1+2*d2+3*d3+...+9*d9) mod 11 = 0


calcChecksum : String -> CustomChecksum
calcChecksum input =
    if String.contains "?" input then
        ChecksumError

    else if String.all Char.isDigit input == False then
        ChecksumError

    else
        let
            charList =
                input
                    |> String.toList

            calc =
                case getIntFromCharList 8 charList of
                    Nothing ->
                        ChecksumError

                    Just char9 ->
                        case getIntFromCharList 7 charList of
                            Nothing ->
                                ChecksumError

                            Just char8 ->
                                case getIntFromCharList 6 charList of
                                    Nothing ->
                                        ChecksumError

                                    Just char7 ->
                                        case getIntFromCharList 5 charList of
                                            Nothing ->
                                                ChecksumError

                                            Just char6 ->
                                                case getIntFromCharList 4 charList of
                                                    Nothing ->
                                                        ChecksumError

                                                    Just char5 ->
                                                        case getIntFromCharList 3 charList of
                                                            Nothing ->
                                                                ChecksumError

                                                            Just char4 ->
                                                                case getIntFromCharList 2 charList of
                                                                    Nothing ->
                                                                        ChecksumError

                                                                    Just char3 ->
                                                                        case getIntFromCharList 1 charList of
                                                                            Nothing ->
                                                                                ChecksumError

                                                                            Just char2 ->
                                                                                case getIntFromCharList 0 charList of
                                                                                    Nothing ->
                                                                                        ChecksumError

                                                                                    Just char1 ->
                                                                                        let
                                                                                            thisSum =
                                                                                                (char9 * 1)
                                                                                                    + (char8 * 2)
                                                                                                    + (char7 * 3)
                                                                                                    + (char6 * 4)
                                                                                                    + (char5 * 5)
                                                                                                    + (char4 * 6)
                                                                                                    + (char3 * 7)
                                                                                                    + (char2 * 8)
                                                                                                    + (char1 * 9)

                                                                                            moddedSum =
                                                                                                if thisSum > 0 then
                                                                                                    modBy 11 thisSum

                                                                                                else
                                                                                                    999999
                                                                                        in
                                                                                        CalcedChecksum moddedSum
        in
        calc


getIntFromCharList : Int -> List Char -> Maybe Int
getIntFromCharList whichOne charList =
    let
        tryChar =
            List.Extra.getAt whichOne charList

        thisString =
            case tryChar of
                Nothing ->
                    Nothing

                Just thisChar ->
                    Just (String.fromChar thisChar)

        thisInt =
            case thisString of
                Nothing ->
                    Nothing

                Just stringFromChar ->
                    let
                        extractedInt =
                            String.toInt stringFromChar

                        outputInt =
                            case extractedInt of
                                Nothing ->
                                    Nothing

                                Just aGoodInt ->
                                    extractedInt
                    in
                    outputInt
    in
    thisInt


type CustomChecksum
    = CalcedChecksum Int
    | ChecksumError
    | ChecksumAmbiguous (List String)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

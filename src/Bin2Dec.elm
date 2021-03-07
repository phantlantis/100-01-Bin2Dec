module Bin2Dec exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Dict exposing (update)
import Binary
import String


-- MAIN
main : Program () Model Msg
main =
    Browser.element 
    { init = \flags -> ( initialModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }

-- MODEL
type alias Model = 
    { input : String,
      result : Int,
      resultVisible : Bool
    }

initialModel : Model
initialModel =
    { input = "", result = 0, resultVisible = False }

-- helper functions for helping manage the string to single binary number conversions
splitElements : String -> List String
splitElements string = 
    String.split "" string

mapBinary : List String -> List Int 
mapBinary inputList =
    case inputList of
        [] -> []
        first :: more -> 
            case toBinary first of
                Ok binary -> binary :: (mapBinary <| more)
                Err msg -> []
toBinary : String -> Result String Int
toBinary singleCharString =
    case String.toInt singleCharString of
        Nothing -> Err "Not a Digit!"
        Just n ->
            case n of
             0 -> Ok 0
             1 -> Ok 1
             _ -> Err "Not a Binary Digit!" 
convertToBinary : String -> Int
convertToBinary inputString =
    splitElements inputString
    |> mapBinary
    |> Binary.fromIntegers
    |> Binary.toDecimal

validateUserInput : String -> Result String String
validateUserInput userInput =
    case String.length userInput of
       0 ->
        Err "Please Enter a Number"
       _ ->
        splitElements userInput
        |> mapBinary
        |> \result -> 
            if List.isEmpty result
            then Err "Invalid Input!"
            else Ok userInput

-- UPDATE

type Msg
    = Change String
    | ClickedCalculate


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
       ClickedCalculate ->
        ({ model | result = convertToBinary model.input, resultVisible = True}, Cmd.none)
       Change newInput ->
        ({ model | input = newInput }, Cmd.none )


-- VIEW
viewConverter : String -> String -> String -> Bool -> String -> Bool -> Html Msg
viewConverter userInput color equivalentDecimal buttonIsDisabled result resultVisible =
    span []
        [ input [value userInput, onInput Change, style "width" "100px"] []
        , br [][] 
        , button [ onClick ClickedCalculate, style "width" "100px", disabled buttonIsDisabled ] [ text "Calculate" ]
        , br [][]
        , span [ style "color" color ] [ text equivalentDecimal ]
        , span [ style "color" "green" ] [ text result ]
        ]

view : Model -> Html Msg
view model =
    case validateUserInput model.input of
       Ok validInput ->
        viewConverter model.input "black" validInput False (String.fromInt model.result) model.resultVisible
       Err msg ->
        viewConverter model.input "red" msg True (String.fromInt model.result) model.resultVisible
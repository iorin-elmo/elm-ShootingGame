module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Json.Decode as D exposing (Decoder, field, string)

import Html exposing (Html, div, text, br)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Svg exposing (Svg, svg, rect, circle)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill, cx, cy, r)
import Time exposing (Posix, every, posixToMillis)
import Task exposing (perform)

type alias Model =
    { pressedKey : String
    , spentTime : Int
    , startTime : Int
    , isPressed :
        { right : Bool
        , left  : Bool
        , back  : Bool
        , front : Bool
        , attack: Bool
        , bomb  : Bool
        }
    , myPosX : Int
    , myPosY : Int
    , numOfBombs : Int
    }

initialModel : Model
initialModel =
    { pressedKey = ""
    , spentTime = 0
    , startTime = 0
    , isPressed =
        { right = False
        , left  = False
        , back  = False
        , front = False
        , attack= False
        , bomb  = False
        }
    , myPosX = 100
    , myPosY = 120
    , numOfBombs = 2
    }

type Msg
    = Pressed Buttons
    | Released Buttons
    | Tick Posix
    | Start Posix

type Buttons
    = Right
    | Left
    | Back
    | Front
    | Attack
    | Bomb
    | None

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed buttons ->
            model
                |> setButtonState buttons True

        Released buttons ->
            model
                |> setButtonState buttons False

        Tick posix ->
            (
                { model |
                  spentTime = (posixToMillis posix) - model.startTime
                , myPosX = model.myPosX
                    - (if model.isPressed.left  then 2 else 0)
                    + (if model.isPressed.right then 2 else 0)
                , myPosY = model.myPosY
                    - (if model.isPressed.front then 2 else 0)
                    + (if model.isPressed.back  then 2 else 0)
                }
                , Cmd.none
            )

        Start posix ->
            ( { model | startTime = posixToMillis posix}
            , Cmd.none
            )


setButtonState: Buttons -> Bool -> Model -> ( Model, Cmd Msg )
setButtonState buttons bool model =
    let
        oldIsPressed = model.isPressed
        newIsPressed =
            case buttons of
                Left  -> { oldIsPressed | left  = bool }
                Right -> { oldIsPressed | right = bool }
                Back  -> { oldIsPressed | back  = bool }
                Front -> { oldIsPressed | front = bool }
                Attack-> { oldIsPressed | attack= bool }
                Bomb  -> { oldIsPressed | bomb  = bool }
                _     -> oldIsPressed
    in
        ( { model | isPressed = newIsPressed }, Cmd.none )

view : Model -> Html Msg
view model =
    let
        myPos =
            [ circle
                [ cx <| String.fromInt model.myPosX
                , cy <| String.fromInt model.myPosY
                , r "5"
                , fill "white"
                ][]
            ]

    in
        div[]
            [ svg[]
                <| List.append
                    [ rect
                        [ x "0"
                        , y "0"
                        , width "200"
                        , height "200"
                        , fill "black"
                        ]
                        []
                    ]
                    myPos
            , text <| (String.fromInt model.myPosX) ++ " : " ++ (String.fromInt model.myPosY)
            ]

stringToButtons : String -> Buttons
stringToButtons str =
    case str of
        "w" -> Front
        "a" -> Left
        "s" -> Back
        "d" -> Right
        "z" -> Attack
        "x" -> Bomb
        _   -> None

keyDownDecoder =
    (field "key" string)
        |> D.map stringToButtons
        |> D.map Pressed

keyUpDecoder =
    (field "key" string)
        |> D.map stringToButtons
        |> D.map Released

subscription : Model -> Sub Msg
subscription model =
    Sub.batch
        [ onKeyDown keyDownDecoder
        , onKeyUp keyUpDecoder
        , Time.every 30 Tick
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_->(initialModel, perform Start Time.now)
        , view = view
        , update = update
        , subscriptions = subscription
        }

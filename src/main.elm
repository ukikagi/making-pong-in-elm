module Main exposing (Input)

import Browser
import Browser.Events
import Collage exposing (Collage, circle, filled, group, rectangle, shift, uniform)
import Collage.Layout
import Collage.Render
import Collage.Text exposing (Typeface(..), color, typeface)
import Color exposing (Color, rgb)
import Html exposing (Html)
import Json.Decode as Decode
import Set exposing (Set)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = KeyDown Key
    | KeyUp Key
    | Tick TimeMs


type alias TimeMs =
    Float


type alias Key =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialGame, Cmd.none )



-- Inputs


type alias Input =
    { space : Bool
    , paddle1 : Int
    , paddle2 : Int
    , delta : TimeMs
    }


keyDecoder : (String -> Msg) -> Decode.Decoder Msg
keyDecoder mapper =
    Decode.field "key" Decode.string
        |> Decode.map mapper


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
        , Browser.Events.onAnimationFrameDelta Tick
        ]



-- Model


gameWidth : Float
gameWidth =
    600


gameHeight : Float
gameHeight =
    400


halfWidth : Float
halfWidth =
    gameWidth / 2


halfHeight : Float
halfHeight =
    gameHeight / 2


type alias Object a =
    { a
        | x : Float
        , y : Float
        , vx : Float
        , vy : Float
    }


type alias Ball =
    Object {}


type alias Player =
    Object { score : Int }


type State
    = Play
    | Pause


type alias Model =
    { keysDown : Set Key
    , state : State
    , ball : Ball
    , player1 : Player
    , player2 : Player
    }


player : Float -> Player
player x =
    { x = x, y = 0, vx = 0, vy = 0, score = 0 }


initialGame : Model
initialGame =
    { keysDown = Set.empty
    , state = Pause
    , ball = { x = 0, y = 0, vx = 200, vy = 200 }
    , player1 = player (20 - halfWidth)
    , player2 = player (halfWidth - 20)
    }



-- Update


near : Float -> Float -> Float -> Bool
near n c m =
    m >= n - c && m <= n + c


within : Ball -> Player -> Bool
within ball player0 =
    near player0.x 8 ball.x
        && near player0.y 20 ball.y


stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
    if lowerCollision then
        abs v

    else if upperCollision then
        -(abs v)

    else
        v


stepObj : TimeMs -> Object a -> Object a
stepObj t ({ x, y, vx, vy } as obj) =
    { obj
        | x = x + vx * t / 1000
        , y = y + vy * t / 1000
    }


stepBall : TimeMs -> Ball -> Player -> Player -> Ball
stepBall t ({ y, vx, vy } as ball) player1 player2 =
    if not (ball.x |> near 0 halfWidth) then
        { ball | x = 0, y = 0 }

    else
        stepObj t
            { ball
                | vx =
                    stepV vx (within ball player1) (within ball player2)
                , vy =
                    stepV vy (y < 7 - halfHeight) (y > halfHeight - 7)
            }


stepPlyr : TimeMs -> Int -> Int -> Player -> Player
stepPlyr t dir points player0 =
    let
        player_ =
            stepObj t { player0 | vy = toFloat dir * 200 }

        y_ =
            clamp (22 - halfHeight) (halfHeight - 22) player_.y

        score_ =
            player0.score + points
    in
    { player_ | y = y_, score = score_ }


updateGame : Input -> Model -> Model
updateGame input game =
    let
        { space, paddle1, paddle2, delta } =
            input

        { state, ball, player1, player2 } =
            game

        score1 =
            if ball.x > halfWidth then
                1

            else
                0

        score2 =
            if ball.x < -halfWidth then
                1

            else
                0

        state_ =
            if space then
                Play

            else if score1 /= score2 then
                Pause

            else
                state

        ball_ =
            if state == Pause then
                ball

            else
                stepBall delta ball player1 player2

        player1_ =
            stepPlyr delta paddle1 score1 player1

        player2_ =
            stepPlyr delta paddle2 score2 player2
    in
    { game
        | state = state_
        , ball = ball_
        , player1 = player1_
        , player2 = player2_
    }


getDirection : String -> String -> Set Key -> Int
getDirection up down keysDown =
    if Set.member down keysDown then
        -1

    else if Set.member up keysDown then
        1

    else
        0


getInput : Model -> TimeMs -> Input
getInput game delta =
    { space = Set.member " " game.keysDown
    , paddle1 = getDirection "w" "s" game.keysDown
    , paddle2 = getDirection "ArrowUp" "ArrowDown" game.keysDown
    , delta = delta
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg game =
    case msg of
        KeyDown key ->
            ( { game | keysDown = Set.insert key game.keysDown }, Cmd.none )

        KeyUp key ->
            ( { game | keysDown = Set.remove key game.keysDown }, Cmd.none )

        Tick delta ->
            let
                input =
                    getInput game delta
            in
            ( updateGame input game, Cmd.none )



-- View


rgbPercent : Int -> Int -> Int -> Color
rgbPercent r g b =
    rgb (toFloat r / 256) (toFloat g / 256) (toFloat b / 256)


pongGreen : Color
pongGreen =
    rgbPercent 60 100 60


textGreen : Color
textGreen =
    rgbPercent 160 200 160


txt : (Collage.Text.Text -> Collage.Text.Text) -> String -> Collage Msg
txt transformer =
    Collage.Text.fromString
        >> color textGreen
        >> typeface Monospace
        >> transformer
        >> Collage.rendered


pauseMessage =
    "SPACE to start, WS and ↑↓ to move"


displayObj : Object a -> Collage.Shape -> Collage Msg
displayObj obj shape =
    shift ( obj.x, obj.y ) (filled (uniform Color.white) shape)


view : Model -> Html Msg
view { state, ball, player1, player2 } =
    let
        scores : Collage Msg
        scores =
            String.fromInt player1.score
                ++ " - "
                ++ String.fromInt player2.score
                |> txt (Collage.Text.size 50)
    in
    group
        [ displayObj ball (circle 7.5)
        , displayObj player1 (rectangle 10 40)
        , displayObj player2 (rectangle 10 40)
        , scores |> shift ( 0, gameHeight / 2 - 40 )
        , (if state == Play then
            Collage.Layout.spacer 1 1

           else
            txt identity pauseMessage
          )
            |> shift ( 0, 40 - gameHeight / 2 )
        , filled (uniform pongGreen) (rectangle gameWidth gameHeight)
        ]
        |> Collage.Render.svg

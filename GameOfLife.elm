import Html exposing (Html, div, span, table, tr, td)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time, second)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  List (List Bool)


-- TODO-4: Add random true elements
init : (Model, Cmd Msg)
init =
  ( List.repeat 20 (List.repeat 20 False)
  , Cmd.none
  )


-- UPDATE

type Msg
  = Tick Time | Set Int Int Bool
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (model, Cmd.none)
    Set x y new_value ->
      let
        set_value: Int -> Int -> Bool -> Bool
        set_value x2 y2 current =
          if x2 == x && y2 == y then
            new_value
          else
            current
      in
        ( List.indexedMap (\x2 r -> (List.indexedMap (\y2 c -> set_value x2 y2 c)) r) model
        , Cmd.none
        )


-- SUBSCRIPTIONS

-- TODO-1 Handle tick event to run the game
subscriptions: Model -> Sub Msg
subscriptions model =
  Time.every second Tick


-- VIEW

-- TODO-2 Add some control (reset, pause, step-by-step, time-travel, resize...)
-- TODO-Make the toggles work as long as the mouse is pressed
view: Model -> Html Msg
view model =
  let
    cell: Int -> Int -> Bool -> Html Msg
    cell x y is_alive =
      td [ style [ ("background-color", if is_alive then "green" else "red")
                  , ("width", "15px")
                  , ("height", "15px")
                  ]
          , onClick (Set x y (not is_alive))
          ] []

    row: Int -> List Bool -> Html Msg
    row x elements =
      tr [] (List.indexedMap (\y c -> cell x y c) elements)

    grid: List (List Bool) -> Html Msg
    grid elements =
      table [] (List.indexedMap (\x r -> row x r) elements)
  in
    div [] [grid model]

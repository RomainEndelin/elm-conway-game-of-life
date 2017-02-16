import Html exposing (Html, div, span, table, tr, td, button, text)
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

type alias Row a =
  List a

type alias Matrix a =
  List (Row a)

type alias Model =
  { matrix: Matrix Bool
  , playing: Bool
  }


-- TODO-4: Add random true elements
init : (Model, Cmd Msg)
init =
  ( { matrix=List.repeat 20 (List.repeat 20 False)
    , playing=False
    }
  , Cmd.none
  )


-- UPDATE

-- map_matrix: Matrix a -> (method -> a -> b) -> Matrix b
-- map_matrix matrix method =
--   ( List.map (\r -> (List.map (\c -> method c)) r) matrix )
--
-- indexed_map_matrix: Matrix a -> (method -> Int -> Int -> a -> b) -> Matrix b
-- indexed_map_matrix matrix method =
--   ( List.map (\r y -> (List.map (\c x -> method x y c)) r) matrix )

type Msg
  = Tick Time | Set Int Int Bool | Toggle
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      let
        zip_row: List a -> List b -> List (a, b)
        zip_row row1 row2 =
          case (row1, row2) of
            ( x :: row1Back, y :: row2Back ) ->
              (x, y) :: zip_row row1Back row2Back

            (_, _) ->
              []

        zip_matrix: Matrix a -> Matrix b -> Matrix (a, b)
        zip_matrix mat1 mat2 =
          case (mat1, mat2) of
            ( row1 :: mat1Back, row2 :: mat2Back ) ->
              (zip_row row1 row2) :: zip_matrix mat1Back mat2Back

            (_, _) ->
              []


        next_generation: Matrix Bool -> Matrix Bool
        next_generation matrix =
          let
            integer_row: List Bool -> List Int
            integer_row row =
              case row of
                cell :: rowBack ->
                  (if cell then 1 else 0) :: integer_row rowBack

                _ ->
                  []

            int_matrix: Matrix Int
            int_matrix =
              List.map (\row -> integer_row row) matrix

            shift_down: Matrix Int -> Matrix Int
            shift_down matrix =
              List.append [List.repeat 20 0] (List.take 19 matrix)

            shift_up: Matrix Int -> Matrix Int
            shift_up matrix =
              List.append (List.drop 1 matrix) [List.repeat 20 0]

            shift_row_left: List Int -> List Int
            shift_row_left row =
              List.append (List.drop 1 row) [0]

            shift_row_right: List Int -> List Int
            shift_row_right row =
              0 :: (List.take 19 row)

            shift_left: Matrix Int -> Matrix Int
            shift_left matrix =
              List.map shift_row_left matrix

            shift_right: Matrix Int -> Matrix Int
            shift_right matrix =
              List.map shift_row_right matrix

            sum_rows: List (Int, Int) -> List Int
            sum_rows rows =
              case rows of
                ( row1, row2 ) :: rowBack ->
                  row1 + row2 :: sum_rows rowBack

                _ ->
                  []

            sum_matrixes: Matrix Int -> Matrix Int -> Matrix Int
            sum_matrixes m1 m2 =
              List.map sum_rows (zip_matrix m1 m2)

            sum_matrixes8: Matrix Int -> Matrix Int -> Matrix Int -> Matrix Int -> Matrix Int -> Matrix Int -> Matrix Int -> Matrix Int -> Matrix Int
            sum_matrixes8 m1 m2 m3 m4 m5 m6 m7 m8 =
              sum_matrixes m1 m2
                |> sum_matrixes m3
                |> sum_matrixes m4
                |> sum_matrixes m5
                |> sum_matrixes m6
                |> sum_matrixes m7
                |> sum_matrixes m8

            neighbors: Matrix Int
            neighbors =
              sum_matrixes8 (shift_up int_matrix) (shift_down int_matrix) (shift_left int_matrix) (shift_right int_matrix) (shift_left (shift_up int_matrix)) (shift_right (shift_up int_matrix)) (shift_left (shift_down int_matrix)) (shift_right (shift_down int_matrix))

            next_generation_row: List (Bool, Int) -> List Bool
            next_generation_row row =
              case row of
                ( cell, neighbors ) :: rowBack ->
                  if neighbors == 3 then
                    True :: next_generation_row rowBack
                  else if neighbors < 2 || neighbors > 3 then
                    False :: next_generation_row rowBack
                  else
                    cell :: next_generation_row rowBack

                _ ->
                  []

            next_generation_matrix: Matrix Bool -> Matrix Int -> Matrix Bool
            next_generation_matrix matrix neighbors =
              -- TODO apply transformation
              List.map next_generation_row (zip_matrix matrix neighbors)
          in
              next_generation_matrix matrix neighbors
      in
        ( {model | matrix=(if model.playing then next_generation model.matrix else model.matrix)}
        , Cmd.none
        )
    Set x y new_value ->
      let
        set_value: Int -> Int -> Bool -> Bool
        set_value x2 y2 current =
          if x2 == x && y2 == y then
            new_value
          else
            current
      in
        ( {model | matrix=List.indexedMap (\x2 r -> (List.indexedMap (\y2 c -> set_value x2 y2 c)) r) model.matrix}
        , Cmd.none
        )
    Toggle ->
      ( {model | playing = not model.playing}
      , Cmd.none
      )


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
  Time.every second Tick


-- VIEW

-- TODO-2 Add some control (reset, step-by-step, time-travel, resize...)
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

    row: Int -> Row Bool -> Html Msg
    row x elements =
      tr [] (List.indexedMap (\y c -> cell x y c) elements)

    grid: Matrix Bool -> Html Msg
    grid elements =
      table [] (List.indexedMap (\x r -> row x r) elements)

    textPlay: Bool -> String
    textPlay isPlaying =
      if isPlaying then "Pause" else "Play"
  in
    div [] [ div [] [grid model.matrix]
           , button [onClick Toggle] [ text (textPlay model.playing)]
           ]

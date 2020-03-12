module Main exposing (..)

import Html exposing (..)
import Browser
import Array exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type Symbol =  X | O | Empty

type alias Moves = Array Int

type alias Player =  {symbol: Symbol, moves: Moves}

type alias Board = Array (Array Symbol)

type alias Game = {current: Player, opponent: Player, board: Board, hasWon: Bool}

winningConditions = [[1,2,3], [4,5,6], [7,8,9], [1,5,9], [3,5,7], [1,4,7], [2,5,8], [3,6,9]]

moves_lookup : Int -> Int -> Int
moves_lookup a b = case (a,b) of
    (0, 0) -> 1
    (0, 1) -> 2
    (0, 2) -> 3
    (1, 0) -> 4
    (1, 1) -> 5
    (1, 2) -> 6
    (2, 0) -> 7
    (2, 1) -> 8
    (2, 2) -> 9
    (_, _) -> 0

hasWon : Moves -> Bool

hasWon moves = List.any (\l -> List.all (\n -> List.member n (Array.toList moves)) l) winningConditions

main =
  Browser.sandbox { init = init, update = update, view = view }

init : Game
init = Game
       (Player X (Array.fromList []))
       (Player O (Array.fromList []))
       (Array.repeat 3 (Array.repeat 3 Empty)) False

toString : Symbol -> String

toString sym = case sym of
    X -> "X"
    O -> "O"
    Empty -> " "

update : List Int -> Game -> Game

update l g = case l of
    [r, c] ->
        (Game
        g.opponent
        {symbol = g.current.symbol, moves = (Array.push (moves_lookup r c) g.current.moves)}
        (Array.set r (Array.set c (.symbol g.current) (Maybe.withDefault (Array.fromList []) (Array.get r g.board))) g.board)
        (hasWon (Array.push (moves_lookup r c) g.current.moves)))
    _ -> g

cell : Int -> Int -> Symbol -> Html (List Int)

cell r column symbol = div [ style "background-color" "#706c61"
                      , style "height" "200px"
                      , style "width" "200px"
                      , style "border" "1px solid white"
                      , style "display" "flex"
                      , style "align-items" "center"
                      , style "justify-content" "center"
                      , style "color" "white"
                      , style "font-size" "50px"
                      , onClick [r, column]
                     ]
                     [text (toString symbol)]

row : Int -> Array Symbol -> Html (List Int)

row index r = div [style "display" "flex"
                 , style "align-items" "center"
                 , style "justify-content" "center"]
                 (Array.toList (Array.indexedMap (cell index) r))

won : Symbol -> Html div
won s = div [style "font-size" "40px",
           style "height" "100vh",
           style "width" "100vw",
           style "border" "1px solid black",
           style "display" "flex",
           style "flex-direction" "column",
           style "justify-content" "center",
           style "align-items" "center",
           style "position" "absolute",
           style "top" "0",
           style "left" "0",
           style "background-color" "#808080bf",
           style "color" "#beebe9",
           style "-webkit-animation" "fadein 2s"
          ] [text ((toString s) ++ " Won!")]

board : Array (Array Symbol) -> Html (List Int)
board b = div []
               [div [] (Array.toList (Array.indexedMap row b))]

heading = div [style "font-size" "40px"] [text "Tic-Tac-Toe"]

game : Array (Array Symbol) -> Html (List Int)
game b = div [style "height" "85vh",
              style "width" "100vw",
              style "display" "flex",
              style "flex-direction" "column",
              style "justify-content" "space-evenly",
              style "align-items" "center"]
              [heading, board b]

view : Game -> Html (List Int)
view g = if (.hasWon g)
                then div [] [game g.board, won (g.opponent.symbol)]
                else div [] [game g.board]

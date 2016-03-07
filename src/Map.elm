module Map where

import Dict exposing (Dict)
import List
import Maybe exposing (Maybe)

import Prism exposing (Prism, prism)

import Games.Roguelike.Tile exposing (Tile)

type alias MapTile =
  { tile : Tile
  , passable : Bool
  }

type alias Map = Dict (Int, Int) MapTile

draw : Map -> Dict (Int, Int) Tile
draw = Dict.map (always .tile)

tile : (Int, Int) -> Prism Map MapTile
tile l = prism (Dict.get l)
               (Dict.insert l)
               (Dict.remove l)

empty : Map
empty = Dict.empty

canMove : (Int, Int) -> Map -> Bool
canMove l m = Maybe.withDefault False <| Maybe.map .passable <| Prism.get (tile l) m

new : Int -> Int -> Tile -> Map
new w h t = List.concatMap (\ x -> List.map (\ y -> ((x, y), MapTile t True)) [0..h]) [0..w] |> Dict.fromList

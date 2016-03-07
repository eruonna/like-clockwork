module Entity where

import Dict exposing (Dict)
import Maybe exposing (Maybe)

import Prism exposing (Prism, prism)
import Games.Roguelike.Tile exposing (Tile)
import Games.Roguelike.Console as Console
import Games.Roguelike.Console exposing (Console)

type alias Entity =
  { pos : Maybe (Int, Int)
  , tile : Maybe Tile
  }

new : Entity
new =
  { pos = Nothing
  , tile = Nothing
  }

pos : Prism Entity (Int, Int)
pos = prism .pos (\ p e -> { e | pos = Just p }) (\ e -> { e | pos = Nothing })

tile : Prism Entity Tile
tile = prism .tile (\ t e -> { e | tile = Just t }) (\ e -> { e | tile = Nothing })

draw : Console -> Entity -> Console
draw c e = case (e.pos, e.tile) of
  (Just p, Just t) -> Console.draw p t c
  _                -> c

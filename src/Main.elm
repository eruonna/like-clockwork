module Main where

import Dict exposing (Dict)
import Html exposing (Html)

import Games.Roguelike.Tile as Tile
import Games.Roguelike.Tile exposing (Rect)
import Games.Roguelike.Console as Console

import Entity exposing (Entity)
import GameState as GS
import Map exposing (Map)
import Prism exposing (Prism)

tilesheet = Tile.tilesheet "../assets/consolas_unicode_16x16.png" 16 16

playerTile = tilesheet 0 2 (Just "@")
floorTile = tilesheet 14 1 (Just ".")

width = 80
height = 40

(newGame, player) =
  let playerEnt = Entity.new |> Prism.set Entity.pos (0, 0)
                             |> Prism.set Entity.tile playerTile
  in GS.empty |> GS.setMap (Map.new width height floorTile)
              |> GS.addEntity playerEnt 

view : GS.GameState -> Html
view gs = Console.fromMap (Rect 0 0 width height) gs.map .tile
                 |> (\ c -> Dict.foldl (\ _ e c -> Entity.draw c e) c gs.entities)
                 |> Console.view

main = view newGame

module Main where

import Dict exposing (Dict)
import Html exposing (Html)
import Signal

import Games.Roguelike.Tile as Tile
import Games.Roguelike.Tile exposing (Rect)
import Games.Roguelike.Console as Console

import Command exposing (Command)
import Entity exposing (Entity)
import GameState as GS
import Input
import Map exposing (Map)
import Messages
import Messaging
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

update : GS.Id -> Command -> GS.GameState -> GS.GameState
update player cmd gs = 
 uncurry (Messaging.runMessages Messages.handleMessage)
  <| GS.runEntityUpdate player (Command.run cmd) gs

view : GS.GameState -> Html
view gs = Console.fromMap (Rect 0 0 width height) gs.map .tile
                 |> (\ c -> Dict.foldl (\ _ e c -> Entity.draw c e) c gs.entities)
                 |> Console.view

theGame = Signal.foldp (update player) newGame Input.commands

main = Signal.map view theGame

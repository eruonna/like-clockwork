module Input where

import Command exposing (Command)
import Movement

import Char
import Dict exposing (Dict)
import Keyboard
import Maybe
import Signal

type alias CommandMap = Dict Char.KeyCode Command

vikeys : CommandMap
vikeys = Dict.fromList [ (Char.toCode 'h', Command.Move Movement.W)
                       , (Char.toCode 'j', Command.Move Movement.S)
                       , (Char.toCode 'k', Command.Move Movement.N)
                       , (Char.toCode 'l', Command.Move Movement.E)
                       , (Char.toCode 'y', Command.Move Movement.NW)
                       , (Char.toCode 'u', Command.Move Movement.NE)
                       , (Char.toCode 'b', Command.Move Movement.SW)
                       , (Char.toCode 'n', Command.Move Movement.SE)
                       ]

theCommandMap : CommandMap
theCommandMap = vikeys

commands : Signal Command
commands =
  Signal.map (\ c -> Maybe.withDefault Command.Unknown <| Dict.get c theCommandMap) Keyboard.presses

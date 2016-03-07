module Games.Roguelike.Tile
  ( Tile
  , Rect
  , mkTile
  , tilesheet
  , view ) where

{-| Representation of square tiles from a tilesheet.

# Types
@docs Rect, Tile

# Create
@docs mkTile, tilesheet

# View
@docs view

-}

import Dict exposing (Dict)
import Html exposing (Html, div, img)
import Html.Attributes exposing (style, alt, src)
import Maybe exposing (Maybe)

{-| A rectangle described by its top left corner and its width and height.
-}
type alias Rect a =
  { x : a
  , y : a
  , w : a
  , h : a
  }

{-| References a tile (or sprite) within a tilesheet.
-}
type alias Tile =
  { sheet : String
  , rect : Rect Int
  , alt : Maybe String
  }

{-| Constructor for `Tile`s.
-}
mkTile : String -> Rect Int -> Maybe String -> Tile
mkTile = Tile

{-| Create a tilesheet with a fixed tile size. This is intended to be used with
partial application.

    consolas16x16 = tilesheet "assets/consolas16x16.png" 16 16

    playerTile = consolas16x16 0 2 (Just "@")
    floorTile = consolas16x16 14 1 (Just ".")
-}
tilesheet : String -> Int -> Int -> Int -> Int -> Maybe String -> Tile
tilesheet sheet w h x y a =
  mkTile sheet (Rect (x*w) (y*h) w h) a

{-| View a tile.
-}
view : Tile -> Html
view t = croppedImage t.rect t.sheet (Maybe.withDefault "" t.alt)

croppedImage : Rect Int -> String -> String -> Html
croppedImage r url a =
  div [ cropDivStyle r.w r.h ]
    [ img [ cropImgStyle r.x r.y, alt a, src url ] [] ]

cropDivStyle : Int -> Int -> Html.Attribute
cropDivStyle w h =
  style [ ("overflow", "hidden")
        , ("padding", "0px")
        , ("margin", "0px")
        , ("width", toString w ++ "px")
        , ("height", toString h ++ "px")
        ]

cropImgStyle : Int -> Int -> Html.Attribute
cropImgStyle x y =
  style [ ("padding", "0px")
        , ("margin", toString (-y) ++ "px 0px 0px " ++ toString (-x) ++ "px")
        ]

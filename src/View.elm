module View exposing (..)

import GraphicSVG exposing(..)
import GraphicSVG.App exposing (..)
import List exposing (map)
import Dict exposing (Dict)
import Model exposing (..)
import Update exposing (..)

-- MAIN FUNCTION --

main =
    gameApp Tick
        { model = init
        , title = "Elm Colouring Game"
        , update = update
        , view = view
        }

-- VIEW --

view model =
    collage 800 500 [ renderSections model.sections |> move (-150,0) -- See
                    , infopanel model.currentShape (Dict.get model.currentShape model.sections) |> move (250,0)
                    , submitButton |> notifyTap CheckAnswer |> move (250,-120)
                    , rect 800 500 |> outlined (solid 8) black]

submitButton = group [rect 100 50 |> filled black
                     ,GraphicSVG.text "Submit" |> centered |> filled white] 

infopanel id cs = case cs of
                  Just a -> let (r1,g1,b1) = a.colour
                                (rt,gt,bt) = a.target
                                (r,g,b) = (toFloat r1, toFloat g1, toFloat b1)
                            in group [rect 300 500 |> filled grey,
                                   text ("Now modifying: " ++ a.title) 
                                      |> centered
                                      |> filled black
                                      |> move (0,200)
                                  ,text ("Target HEXCODE: #" ++ toHex rt ++ toHex gt ++ toHex bt) 
                                      |> centered
                                      |> filled black
                                      |> move (0,180)
                                  ,square 40 
                                      |> filled (rgb r g b)
                                      |> addOutline (solid 2) black
                                      |> move (0,150)
                                  ,GraphicSVG.text (currentColorText (r1,g1,b1))
                                      |> centered
                                      |> filled black
                                      |> move (0,115)
                                  ,sliders (r,g,b) id]
                  Nothing -> group [rect 300 500 |> filled grey]

currentColorText (r,g,b) = "Current RGB: (" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"

sliders (r,g,b) id = group [ slider Red r id,
                             slider Green g id |> move (0,-35),
                             slider Blue b id |> move (0,-70)]

slider: ColorValue -> Float -> Int -> Shape Mesg
slider c v id = let (darkclr, clr) = case c of 
                                 Red -> (darkRed, red)
                                 Green -> (darkGreen, green)
                                 Blue -> (darkBlue, blue)
                in group [rect (200*v/255) 20 |> filled clr |> move (100*v/255-100,0)
                     , rect 200 20 |> outlined (solid 1) darkclr
                     , plusButton darkclr clr 
                         |> move (120,0) 
                         |> notifyMouseDown (Increase c id)
                         |> notifyTouchStart(Increase c id)
                         |> notifyMouseUp ReleaseButton
                         |> notifyTouchEnd ReleaseButton
                     , minusButton darkclr clr |> move (-120,0)
                         |> notifyMouseDown (Decrease c id)
                         |> notifyTouchStart (Decrease c id)
                         |> notifyMouseUp ReleaseButton
                         |> notifyTouchEnd ReleaseButton]


plusButton darkclr clr = group [square 20 |> filled clr |> addOutline (solid 1) darkclr,
                                polygon [(-8,2),(-2,2),(-2,8),(2,8),(2,2),(8,2),(8,-2),(2,-2),(2,-8),(-2,-8),(-2,-2),(-8,-2)] |> filled darkclr]

minusButton darkclr clr = group [square 20 |> filled clr |> addOutline (solid 1) darkclr,
                                 rect 16 4 |> filled darkclr]


--## HELPER FUNCTIONS ##--

--# Rendering the Sections #--

renderSections: Dict Int Section -> Shape Mesg
renderSections sections = group (List.map renderSection (Dict.toList sections))

renderSection : (Int,Section) -> Shape Mesg
renderSection (id,s) = let (r,g,b) = s.colour
                       in s.shapes (rgb (toFloat r) (toFloat g) (toFloat b)) 
                            |> notifyTap (UpdateCurrentShape id)    

--# Hex Conversion #--

toHex: Int -> String
toHex x = let a = x // 16
              b = modBy 16 x
          in hexDigit a ++ hexDigit b

hexDigit: Int -> String -- Helper function
hexDigit x = case x of
               10 -> "A"
               11 -> "B"
               12 -> "C"
               13 -> "D"
               14 -> "E"
               15 -> "F"
               _ -> String.fromInt x
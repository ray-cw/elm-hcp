module View exposing (..)

import GraphicSVG.App exposing (..)
import GraphicSVG exposing(..)
import List exposing (map)
import Dict exposing (Dict)
import Model exposing (..)
import Update exposing (..)

-- Modification to the text to make it more readable for me lol --

text a = GraphicSVG.text a |> sansserif |> size 11.5

-- Clarification that we are using Model.Msg instead of GraphicSVG.Msg

type alias Msg = Model.Msg

-- MAIN FUNCTION --

main =
    gameApp Tick
        { model = init
        , title = "Elm Colouring Game"
        , update = update
        , view = view
        }

-- VIEW --

view model = case model.mode of
              InstructionScreen ->
                          collage 800 500 [ renderSections model.sections |> move (-150,0)
                                          , infopanel model.currentShape (Dict.get model.currentShape model.sections) |> move (250,0)
                                          , rect 800 500 |> outlined (solid 8) black
                                          , rect 400 250 |> filled (rgba 0 0 0 0.75)
                                          , text "Please read all the instructions before starting." 
                                                 |> bold
                                                 |> filled white                                                 
                                                 |> move (-190,100)
                                          , text "Your goal: adjust the colors of the shapes on the left side of the screen to" 
                                                 |> filled white
                                                 |> move (-190,80)
                                          , text "match the hexcodes provided on the panel on the right side of the screen." 
                                                 |> filled white
                                                 |> move (-190,65)
                                          , text "Select a shape by tapping on it. Adjust colors using the sliders that pop-up." 
                                                 |> filled white
                                                 |> move (-190,45)
                                          , text "You can hold down the slider buttons to go faster!" 
                                                 |> filled white
                                                 |> move (-190,30)
                                          , text "Remember in hex: A = 10, B = 11, C = 12, D = 13, E = 14, and F = 15." 
                                                 |> filled white
                                                 |> move (-190,0)
                                          , text "Examples: FF = 15 * 16 + 15 = 255, AD = 10 * 16 + 13 = 173" 
                                                 |> filled white
                                                 |> move (-190,-45)
                                          , text "Click anywhere to start." 
                                                 |> filled white
                                                 |> move (-190,-90)
                                          , rect 800 500 |> filled (rgba 0 0 0 0) |> notifyTap StartGame]
              GameScreen -> collage 800 500 [ renderSections model.sections |> move (-150,0)
                                            , infopanel model.currentShape (Dict.get model.currentShape model.sections) |> move (250,0)
                                            , questionButton |> move (380,230) |> notifyTap OpenInstructions
                                            , rect 800 500 |> outlined (solid 8) black]
              VictoryScreen -> collage 800 500 [ text "You did it yay" 
                                                     |> filled black
                                                     |> move (0,20)
                                              , text "Click anywhere to restart." |> filled black
                                              , rect 800 500 |> filled (rgba 0 0 0 0) |> notifyTap Reset]

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
                                  ,text (currentColorText (r1,g1,b1))
                                      |> centered
                                      |> filled black
                                      |> move (0,115)
                                  , sliders (r,g,b) id a.resultString
                                  , submitButton a.resultString |> move (0,-120) |> notifyTap CheckAnswer]
                  Nothing -> instructionScreen

questionButton = group [ square 20 |> filled (rgb 30 30 30) |> addOutline (solid 2) black,
                         text "Click this for help >>" |> size 10 |> filled black |> move (-105,-4),
                         text "?" |> centered |> filled white |> move (0,-4)]

submitButton s = if s /= Just "Correct! Keep at it!" then
                  group [rect 60 30 |> filled white |> addOutline (solid 1) red
                        , text "Submit" |> sansserif |>  centered |> filled red |> move (0,-4)
                        , showResultString s |> move (0,-40)] 
                 else group [showResultString s |> move (0,-40)]

instructionScreen: Shape Msg
instructionScreen = group [rect 300 500 |> filled grey]

currentColorText (r,g,b) = "Current RGB: (" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"

showResultString: Maybe String -> Shape Msg
showResultString s = case s of
                        Just a -> text a |> centered |> filled black
                        Nothing -> group[]

sliders (r,g,b) id s = if s /= Just "Correct! Keep at it!" then
                            group [ slider Red r id,
                                    slider Green g id |> move (0,-35),
                                    slider Blue b id |> move (0,-70)]
                       else group []

slider: ColorValue -> Float -> Int -> Shape Msg
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
                         |> notifyLeave ReleaseButton
                     , minusButton darkclr clr |> move (-120,0)
                         |> notifyMouseDown (Decrease c id)
                         |> notifyTouchStart (Decrease c id)
                         |> notifyMouseUp ReleaseButton
                         |> notifyTouchEnd ReleaseButton
                         |> notifyLeave ReleaseButton]


plusButton darkclr clr = group [square 20 |> filled clr |> addOutline (solid 1) darkclr,
                                polygon [(-8,2),(-2,2),(-2,8),(2,8),(2,2),(8,2),(8,-2),(2,-2),(2,-8),(-2,-8),(-2,-2),(-8,-2)] |> filled darkclr]

minusButton darkclr clr = group [square 20 |> filled clr |> addOutline (solid 1) darkclr,
                                 rect 16 4 |> filled darkclr]


--## HELPER FUNCTIONS ##--

--# Rendering the Sections #--

renderSections: Dict Int Section -> Shape Msg
renderSections sections = group (List.map renderSection (Dict.toList sections))

renderSection : (Int,Section) -> Shape Msg
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
module Test exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import GraphicSVG exposing(..)
import GraphicSVG.App exposing (..)
import Http exposing (..)
import List exposing (map)
import Dict exposing (Dict)

type Msg
    = Tick Float GetKeyState 
    | UpdateCurrentShape Int
    | Increase ColorValue Int 
    | Decrease ColorValue Int
    | ReleaseButton
    | CheckAnswer

type Mode = Inc | Dec

type ColorValue = Red | Green | Blue

type alias Model = { sections: Dict Int Section, 
                     currentShape: Int, -- ID of current shape, -1 by default
                     activeSlider: Maybe (Int,Mode,ColorValue),
                     timeActive: Float,
                     resultMsg: Maybe String} -- Keeps track of time button was pressed

init = { sections = sections, currentShape = -1, activeSlider = Nothing, timeActive = 0, resultMsg = Nothing}

base = (255,255,255) -- starting colour is white

rgbcodes : List ((Int,Int,Int),(Int,Int,Int))
rgbcodes = [(base,(120,100,100))
           ,(base,(120,100,100))
           ,(base,(120,100,100))
           ,(base,(120,100,100))] -- (starting colour, target colour)

type alias Section = { title: String
                     , colour: (Int, Int, Int)
                     , target: (Int, Int, Int)
                     , shapes: Color -> Shape Msg}

sections : Dict Int Section
sections = Dict.fromList
           [ (0, {title = "Sky", colour = base, target = (2,10,10), shapes = sky})
           , (1, {title = "Hill", colour = (50,60,70), target = (10,10,10), shapes = hill})
           , (2, {title = "Sun", colour = (100,200,10), target = (10,10,10), shapes = sun})]


sun c  = group [circle 50 
                  |> filled c
                  |> addOutline (solid 1) black
                  |> move (100,100)
                  ]

hill c = circle 500 |> filled c
                    |> addOutline (solid 1) black
                    |> move (0,-500)


sky c = square 1000 |> filled c


update msg model =
    case msg of
        Tick t ( _, _, _ ) ->
           if model.activeSlider == Nothing then model else
           if model.timeActive > 2 then
                          case model.activeSlider of
                           Just (id,m,c) -> case m of
                                               Inc -> { model | sections = Dict.update id (increaseWithDefault c) model.sections}
                                               Dec -> { model | sections = Dict.update id (decreaseWithDefault c) model.sections}
                           Nothing -> model
           else {model | timeActive = model.timeActive + 0.1}
        UpdateCurrentShape id -> {model | currentShape = id}
        Increase c id -> { model | sections = Dict.update id (increaseWithDefault c) model.sections, activeSlider = Just (id,Inc,c)}
        Decrease c id -> { model | sections = Dict.update id (decreaseWithDefault c) model.sections, activeSlider = Just (id,Dec,c)}
        ReleaseButton -> { model | activeSlider = Nothing, timeActive = 0 }
        CheckAnswer -> model

decreaseWithDefault: ColorValue -> Maybe Section -> Maybe Section
decreaseWithDefault c s = case s of 
                         Just a -> let (r,g,b) = a.colour
                                   in case c of
                                      Red -> Just {a | colour = (max (r-1) 0,g,b)}
                                      Green -> Just {a | colour = (r,max (g-1) 0,b)}
                                      Blue -> Just {a | colour = (r,g,max (b-1) 0)}
                         Nothing -> Nothing

increaseWithDefault: ColorValue -> Maybe Section -> Maybe Section
increaseWithDefault c s = case s of 
                         Just a -> let (r,g,b) = a.colour
                                   in case c of
                                      Red -> Just {a | colour = (min (r+1) 255,g,b)}
                                      Green -> Just {a | colour = (r,min (g+1) 255,b)}
                                      Blue -> Just {a | colour = (r,g,min (b+1) 255)}
                         Nothing -> Nothing

view model =
    collage 800 500 [ image model |> move (-150,0)
                    , infopanel model.currentShape (Dict.get model.currentShape model.sections) |> move (250,0)
                    , submitButton |> notifyTap CheckAnswer |> move (250,-120)
                    , rect 800 500 |> outlined (solid 8) black]


submitButton = group [rect 100 50 |> filled black
                     ,GraphicSVG.text "Submit" |> centered |> filled white] 

image model = group (List.map renderSection (Dict.toList model.sections))

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

infopanel id cs = case cs of
                  Just a -> let (r1,g1,b1) = a.colour
                                (rt,gt,bt) = a.target
                                (r,g,b) = (toFloat r1, toFloat g1, toFloat b1)
                            in group [rect 300 500 |> filled grey,
                                   GraphicSVG.text ("Now modifying: " ++ a.title) 
                                      |> centered
                                      |> filled black
                                      |> move (0,200)
                                  ,GraphicSVG.text ("Target HEXCODE: #" ++ toHex rt ++ toHex gt ++ toHex bt) 
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
                  Nothing -> rect 300 500 |> filled grey

currentColorText (r,g,b) = "Current RGB: (" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"

sliders (r,g,b) id = group [ slider Red r id,
                             slider Green g id |> move (0,-35),
                             slider Blue b id |> move (0,-70)]

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
                     , minusButton darkclr clr |> move (-120,0)
                         |> notifyMouseDown (Decrease c id)
                         |> notifyTouchStart (Decrease c id)
                         |> notifyMouseUp ReleaseButton
                         |> notifyTouchEnd ReleaseButton]


plusButton darkclr clr = group [square 20 |> filled clr |> addOutline (solid 1) darkclr,
                                polygon [(-8,2),(-2,2),(-2,8),(2,8),(2,2),(8,2),(8,-2),(2,-2),(2,-8),(-2,-8),(-2,-2),(-8,-2)] |> filled darkclr]

minusButton darkclr clr = group [square 20 |> filled clr |> addOutline (solid 1) darkclr,
                                 rect 16 4 |> filled darkclr]

renderSection : (Int,Section) -> Shape Msg
renderSection (id,s) = let (r,g,b) = s.colour
                       in s.shapes (rgb (toFloat r) (toFloat g) (toFloat b)) |> notifyTap (UpdateCurrentShape id)    

main =
    gameApp Tick
        { model = init
        , title = "Elm Colouring Game"
        , update = update
        , view = view
        }


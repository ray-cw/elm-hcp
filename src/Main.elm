module Test exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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

type ColorValue = Red | Green | Blue

type alias Model = { angle : Float, 
                     speed : Float, 
                     sections: Dict Int Section, 
                     currentID: Maybe Int,
                     currentShape: Maybe Section, 
                     activeSlider: Maybe ColorValue}

init = { angle = 0, speed = 1, sections = sections, currentID = Nothing, currentShape = Nothing, activeSlider = Nothing}

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
        Tick _ ( keys, _, _ ) ->
            case keys (Key "r") of
                JustDown ->
                    { model
                    | angle = model.angle - model.speed
                    , speed = -model.speed
                    }
                _ -> { model | angle = model.angle + model.speed }
        UpdateCurrentShape id -> {model | currentShape = Dict.get id model.sections}
        Increase c id ->
            case c of
                _ -> model
        Decrease c id ->
            case c of 
                Red -> { model | sections = Dict.update id updateWithDefault model.sections}
                _ -> model

updateWithDefault: Maybe Section -> Maybe Section
updateWithDefault s = case s of 
                        Just a -> let (r,g,b) = a.colour
                                  in Just {a | colour = (r-1,g,b)}
                        Nothing -> Nothing


view model =
    collage 800 500 [ image model |> move (-150,0)
                    , infopanel model.currentShape |> move (250,0)
                    , rect 800 500 |> outlined (solid 8) black]

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

infopanel cs = case cs of
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
                                  ,sliders (r,g,b)]
                  Nothing -> rect 300 500 |> filled grey

currentColorText (r,g,b) = "Current RGB: (" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"

sliders (r,g,b) = group [ slider Red r,
                          slider Green g |> move (0,-35),
                          slider Blue b |> move (0,-70)]

slider: ColorValue -> Float -> Shape Msg
slider c v = let (darkclr, clr) = case c of 
                                 Red -> (darkRed, red)
                                 Green -> (darkGreen, green)
                                 Blue -> (darkBlue, blue)
             in group [rect (200*v/255) 20 |> filled clr |> move (100*v/255-100,0)
                     , rect 200 20 |> outlined (solid 1) darkclr
                     , plusButton darkclr clr |> move (120,0)
                     , minusButton darkclr clr |> move (-120,0)]


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


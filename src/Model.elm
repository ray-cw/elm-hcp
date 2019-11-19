module Model exposing (..)

import Dict exposing (Dict)
import GraphicSVG exposing(..)
import GraphicSVG.App exposing (GetKeyState)

-- HELPER TYPES --

base: (Int,Int,Int)
base = (255,255,255) -- starting colour is white

type Mesg
    = Tick Float GetKeyState 
    | UpdateCurrentShape Int
    | Increase ColorValue Int 
    | Decrease ColorValue Int
    | ReleaseButton
    | CheckAnswer

type Mode = Inc | Dec

type ColorValue = Red | Green | Blue

type alias Section = { title: String
                     , colour: (Int, Int, Int)
                     , target: (Int, Int, Int)
                     , shapes: Color -> Shape Mesg}

-- MODEL TYPE --

type alias Model = { sections: Dict Int Section, 
                     currentShape: Int, -- ID of current shape, -1 by default
                     activeSlider: Maybe (Int,Mode,ColorValue),
                     timeActive: Float,
                     resultMsg: Maybe String} -- Keeps track of time button was pressed

-- INITIAL STATE --

init = { sections = sections, 
         currentShape = -1, 
         activeSlider = Nothing, 
         timeActive = 0, 
         resultMsg = Nothing}

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
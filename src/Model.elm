module Model exposing (..)

import Dict exposing (Dict)
import GraphicSVG exposing(..)
import GraphicSVG.App exposing (GetKeyState)

-- HELPER TYPES --

base: (Int,Int,Int)
base = (255,255,255) -- starting colour is white


type Msg
    = Tick Float GetKeyState 
    | UpdateCurrentShape Int
    | Increase ColorValue Int 
    | Decrease ColorValue Int
    | ReleaseButton
    | CheckAnswer
    | StartGame
    | OpenInstructions
    | Reset

type Mode = Inc | Dec

type ColorValue = Red | Green | Blue

type Screen = InstructionScreen | GameScreen | VictoryScreen

type alias Section = { title: String
                     , colour: (Int, Int, Int)
                     , target: (Int, Int, Int)
                     , shapes: Color -> Shape Msg
                     , resultString: Maybe String}

type alias Scorecard = { nOT : Int -- Number of Trials
                       , nOS : Int  -- Number of Sections 
                       , nOC: Int } -- Number of Correct
-- MODEL TYPE --

type alias Model = { sections: Dict Int Section
                   , currentShape: Int -- ID of current shape, -1 by default
                   , activeSlider: Maybe (Int,Mode,ColorValue)
                   , timeActive: Float -- Keeps track of time button was pressed
                   , scorecard : Scorecard
                   , mode: Screen
                   } 

-- INITIAL STATE --

init = { sections = sections, 
         currentShape = -1, 
         activeSlider = Nothing, 
         timeActive = 0,
         scorecard = {nOT = 0, nOS = Dict.size sections, nOC = 0}
       , mode = InstructionScreen}


{-- Actual version

sections : Dict Int Section
sections = Dict.fromList
           [ (0, {title = "Sky", colour = base, target = (205,234,241), shapes = sky, resultString = Nothing})
           , (1, {title = "Left Hill", colour = base, target = (152,206,130), shapes = lefthill, resultString = Nothing})
           , (2, {title = "Right Hill", colour = base, target = (202,255,183), shapes = righthill, resultString = Nothing})
           , (3, {title = "Cloud", colour = (0,0,0), target = base, shapes = cloud, resultString = Nothing})
           , (4, {title = "Sun", colour = base, target = (244,242,133), shapes = sun, resultString = Nothing})]

--}



-- Testing version --
sections : Dict Int Section
sections = Dict.fromList
           [ (0, {title = "Sky", colour = (205,234,241), target = (205,234,241), shapes = sky, resultString = Nothing})
           , (1, {title = "Left Hill", colour = (152,206,130), target = (152,206,130), shapes = lefthill, resultString = Nothing})
           , (2, {title = "Right Hill", colour = (202,255,183), target = (202,255,183), shapes = righthill, resultString = Nothing})
           , (3, {title = "Cloud", colour = base, target = base, shapes = cloud, resultString = Nothing})
           , (4, {title = "Sun", colour = (244,242,133), target = (244,242,133), shapes = sun, resultString = Nothing})]





sun c  = group [circle 50 
                  |> filled c
                  |> addOutline (solid 1) black
                  |> move (100,100)
                  ]

lefthill c = circle 500 |> filled c
                    |> addOutline (solid 1) black
                    |> move (-100,-550)

righthill c = circle 500 |> filled c
                    |> addOutline (solid 1) black
                    |> move (100,-560)


cloud c = curve (0,0) [Pull (-22,60) (-76,19), Pull (-156,72) (-128,3), Pull (-164,-30) (-109,-28), Pull (-79,-58) (-55,-12), Pull (-27,-44) (0,0) ]
                   |> filled c
                   |> addOutline (solid 1) black
                   |> move (-50,180)

sky c = square 1000 |> filled c


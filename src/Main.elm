{--




--}


module Main exposing (..)

import GraphicSVG.EllieApp exposing (..)
import GraphicSVG exposing(..)
import List exposing (map)
import Dict exposing (Dict)
import Model exposing (..)
import Update exposing (..)

-- Modification to the text to make it more readable for me lol --

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

text a = GraphicSVG.text a |> sansserif |> size 11.5

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
renderSections xs = group (List.map renderSection (Dict.toList xs))

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

update: Msg -> Model -> Model
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
        StartGame -> {model | mode = GameScreen}
        OpenInstructions -> {model | mode = InstructionScreen}
        UpdateCurrentShape id -> {model | currentShape = id}
        Increase c id -> { model | sections = Dict.update id (increaseWithDefault c) model.sections, activeSlider = Just (id,Inc,c)}
        Decrease c id -> { model | sections = Dict.update id (decreaseWithDefault c) model.sections, activeSlider = Just (id,Dec,c)}
        ReleaseButton -> { model | activeSlider = Nothing, timeActive = 0 }
        CheckAnswer -> let id = model.currentShape
                           currentSection = Dict.get id model.sections
                       in case currentSection of
                             Just s -> let colour = s.colour
                                           target = s.target
                                       in  if colour == target then
                                               { model | sections = Dict.update id correctAnswer model.sections
                                                       , scorecard = addToScorecard True model.scorecard} 
                                                       |> isComplete
                                           else { model | sections = Dict.update id wrongAnswer model.sections
                                                        , scorecard = addToScorecard False model.scorecard}
                             Nothing -> model 
        Reset -> init

addToScorecard: Bool -> Scorecard -> Scorecard
addToScorecard x sc = case x of 
                         True -> {sc | nOT = sc.nOT + 1, nOC = sc.nOC + 1}
                         False -> {sc | nOT = sc.nOT + 1}

isComplete: Model -> Model
isComplete m = if m.scorecard.nOS == m.scorecard.nOC then {m | mode = VictoryScreen} else m


correctAnswer: Maybe Section -> Maybe Section
correctAnswer sec = case sec of
                   Just s -> Just {s | resultString = Just "Correct! Keep at it!"}
                   Nothing -> Nothing

wrongAnswer: Maybe Section -> Maybe Section
wrongAnswer sec = case sec of
                   Just s -> Just {s | resultString = Just "Oops, that wasn't quite right."}
                   Nothing -> Nothing


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

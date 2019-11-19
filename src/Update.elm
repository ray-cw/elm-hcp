module Update exposing (..)

import Model exposing (..)
import Dict exposing (Dict)

update: Mesg -> Model -> Model
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

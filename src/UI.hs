module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Monomer
import Monomer.Graph

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 16]
        [ graphWithData_ points
            [ lockX_ $ model ^. xLock
            , lockY_ $ model ^. yLock
            , onRightClick AppAddPoint
            ] `nodeKey` "mainGraph"
        , separatorLine
        , vstack'
            [ button "Reset" AppResetGraph
            , hgrid_ [childSpacing_ 64]
                [ labeledCheckbox "Lock X" xLock
                , labeledCheckbox "Lock Y" yLock
                ]
            , separatorLine
            , dropdown_ currentFunction functionChoices et et
                [onChange AppFunctionChange]
            ]
        ] `styleBasic` [padding 16]
    points =
        [
            [ graphPoints $ model ^. dataPoints
            , graphColor black
            , graphSeparate
            , graphOnChange AppPointChange
            ]
        , if null (model ^. currentFunction)
            then []
            else
                [ graphPoints $ (\x -> (x, cf x)) <$> xs
                , graphColor brown
                ]
        ]
    functionChoices = Nothing:(pure <$> [0..length functions-1])
    cf = fst $ functions!!(fromJust $ model ^. currentFunction)
    et i = label $ if null i
        then "No function"
        else snd $ functions!!(fromJust i)
    xs = [-15, (-14.95)..15]
    vstack' = vstack_ [childSpacing_ 16]

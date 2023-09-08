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
            , separatorLine
            , vstack'
                [ label "Add points with right mouse button"
                , vscroll $ vstack' $
                    [ hgrid'
                        [ label "X:"
                        , label "Y:"
                        ]
                    ] <> pointPanels
                ]
            ] `styleBasic` [sizeReqW $ fixedSize 320]
        ] `styleBasic` [padding 16]
    points =
        [
            [ graphPoints ps
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
    pointPanels = makePointPanel <$> [0..length ps-1]
    makePointPanel i = hgrid'
        [ numericField_ (pointField i . _1)
            [ decimals 3
            , onChange $ \x -> AppPointChange i (x, psy!!i)
            ]
        , numericField_ (pointField i . _2)
            [ decimals 3
            , readOnly_ $ not $ null $ model ^. currentFunction
            ]
        ]
    pointField i = lens getter setter where
        getter = (^?! ix i) . _amDataPoints
        setter = flip $ set $ dataPoints . ix i
    functionChoices = Nothing:(pure <$> [0..length functions-1])
    cf = fst $ functions!!(fromJust $ model ^. currentFunction)
    et i = label $ if null i
        then "No function"
        else snd $ functions!!(fromJust i)
    xs = [-15, (-14.95)..15]
    ps = model ^. dataPoints
    (_, psy) = unzip ps
    vstack' = vstack_ [childSpacing_ 16]
    hgrid' = hgrid_ [childSpacing_ 16]

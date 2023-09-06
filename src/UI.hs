module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.Graph

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 16]
        [ graphWithData_ points
            [ lockX_ $ model ^. xLock
            , lockY_ $ model ^. yLock
            ] `nodeKey` "mainGraph"
        , separatorLine
        , vstack_ [childSpacing_ 16]
            [ button "Reset" AppResetGraph
            , hgrid_ [childSpacing_ 64]
                [ labeledCheckbox "Lock X" xLock
                , labeledCheckbox "Lock Y" yLock
                ]
            ]
        ] `styleBasic` [padding 16]
    points =
        [
            [ graphPoints $ (\x -> (x, cos x)) <$> xs
            , graphColor red
            ]
        ]
    xs = [-10, -9.98..10]

module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Monomer
import Monomer.Graph

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetGraph
    | AppAddPoint (Double, Double)
    | AppPointChange Int (Double, Double)
    deriving (Eq, Show)

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetGraph -> [Message "mainGraph" GraphReset]
    AppAddPoint p -> [Model $ model & dataPoints %~ (p:)]
    AppPointChange i p -> [Model $ model & dataPoints . ix i .~ p]

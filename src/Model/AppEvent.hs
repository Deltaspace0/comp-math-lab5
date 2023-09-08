module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.Maybe
import Monomer
import Monomer.Graph
import TextShow

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetGraph
    | AppAddPoint (Double, Double)
    | AppPointChange Int (Double, Double)
    | AppFunctionChange (Maybe Int)
    | AppRemovePoints
    | AppPointClicked Int
    deriving (Eq, Show)

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetGraph -> [Message "mainGraph" GraphReset]
    AppAddPoint p ->
        [ Model $ model & dataPoints %~ ((applyFunction model p):)
        ]
    AppPointChange i p ->
        [ Model $ model & dataPoints . ix i .~ applyFunction model p
        ]
    AppFunctionChange _ ->
        [ Model $ model & dataPoints %~ fmap (applyFunction model)
        ]
    AppRemovePoints -> [Model $ model & dataPoints .~ []]
    AppPointClicked i -> [SetFocusOnKey $ WidgetKey $ showt i]

applyFunction :: AppModel -> (Double, Double) -> (Double, Double)
applyFunction model p@(x, _) = newPoint where
    newPoint = if null cf
        then p
        else (x, fst (functions!!(fromJust cf)) x)
    cf = model ^. currentFunction

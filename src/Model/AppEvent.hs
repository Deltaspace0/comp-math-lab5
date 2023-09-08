module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.List (sort)
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
    | AppDistributePoints Int Bool
    | AppRedistributePoints Bool
    | AppStepChange Double
    deriving (Eq, Show)

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetGraph -> [Message "mainGraph" GraphReset]
    AppAddPoint p ->
        [ Model $ model & dataPoints %~ ((applyFunction model p):)
        , Event $ AppRedistributePoints $ model ^. fixedStep
        ]
    AppPointChange i p ->
        [ Model $ model & dataPoints . ix i .~ applyFunction model p
        , Event $ AppDistributePoints i $ model ^. fixedStep
        ]
    AppFunctionChange _ ->
        [ Model $ model & dataPoints %~ fmap (applyFunction model)
        ]
    AppRemovePoints -> [Model $ model & dataPoints .~ []]
    AppPointClicked i -> [SetFocusOnKey $ WidgetKey $ showt i]
    AppDistributePoints i v -> [Model newModel] where
        newModel = model & dataPoints .~ d
        d = if v && (length dp > 1)
            then applyFunction model <$> zipWith const np dp
            else dp
        np = (dp!!0):[(fst (np!!j) + h, dpy!!(j+1)) | j <- [0..]]
        h = if i == 0
            then dpx!!1 - dpx!!0
            else (dpx!!i - dpx!!0)/(fromIntegral i)
        (dpx, dpy) = unzip dp
        dp = model ^. dataPoints
    AppRedistributePoints v -> response where
        response = if v
            then
                [ Model $ model & dataPoints %~ sort
                , Event $ AppDistributePoints (length dp - 1) v
                ]
            else []
        dp = model ^. dataPoints
    AppStepChange h ->
        [ Model $ model & dataPoints . ix 1 . _1 .~ x
        , Event $ AppDistributePoints 1 True
        ] where x = (fst $ head $ model ^. dataPoints) + h

applyFunction :: AppModel -> (Double, Double) -> (Double, Double)
applyFunction model p@(x, _) = newPoint where
    newPoint = if null cf
        then p
        else (x, fst (functions!!(fromJust cf)) x)
    cf = model ^. currentFunction

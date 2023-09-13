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
    | AppRemovePoint Int
    | AppRemovePoints
    | AppPointClicked Int
    | AppDistributePoints Int Bool
    | AppRedistributePoints Bool
    | AppStepChange Double
    | AppInterpolate
    | AppSearchXChange (Double, Double)
    | AppMethodChange Method
    deriving (Eq, Show)

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> if model ^. instantInter
        then [Event AppInterpolate]
        else []
    AppResetGraph -> [Message "mainGraph" GraphReset]
    AppAddPoint p ->
        [ Model $ model & dataPoints %~ ((applyFunction model p):)
        , Event $ AppRedistributePoints $ model ^. fixedStep
        , Event AppInit
        ]
    AppPointChange i p ->
        [ Model $ model & dataPoints . ix i .~ applyFunction model p
        , Event $ AppDistributePoints i $ model ^. fixedStep
        , Event AppInit
        ]
    AppFunctionChange _ ->
        [ Model $ model & dataPoints %~ fmap (applyFunction model)
        , Event AppInit
        ]
    AppRemovePoint i ->
        [ Model $ model & dataPoints %~ rp
        , Event $ AppRedistributePoints $ model ^. fixedStep
        , Event AppInit
        ] where rp ps = (take i ps) <> (tail $ drop i ps)
    AppRemovePoints ->
        [ Model $ model & dataPoints .~ []
        , Event AppInit
        ]
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
        , Event AppInit
        ] where x = (fst $ head $ model ^. dataPoints) + h
    AppInterpolate -> case model ^. currentMethod of
        Lagrange ->
            [ Model $ model
                & interPolynomial .~ lagrangePolynomial
                & interGraph .~ lagrangeGraph
                & searchSolution .~ interL sx
            ]
        Newton ->
            [ Model $ model
                & interGraph .~ newtonGraph
                & forwardDifferences .~ differences
                & searchSolution .~ newtonF sx
            ]
        Gauss ->
            [ Model $ model
                & interGraph .~ gaussGraph
                & forwardDifferences .~ differences
                & searchSolution .~ gaussF sx
            ]
        where
            lagrangePolynomial = interpolateLagrange dp
            interL = getInterFunction lagrangePolynomial
            lagrangeGraph = (\x -> (x, interL x)) <$> xs
            newtonGraph = (\x -> (x, newtonF x)) <$> xs
            gaussGraph = (\x -> (x, gaussF x)) <$> xs
            xs = [-15, (-14.95)..15]
            newtonF = interpolateNewton dp differences
            gaussF = interpolateGauss dp differences
            differences = getDifferences dp
            sx = model ^. searchX
            dp = model ^. dataPoints
    AppSearchXChange (x, _) ->
        [ Model $ model & searchX .~ x
        , Event AppInit
        ]
    AppMethodChange method -> if method == Lagrange
        then [Event AppInit]
        else
            [ Model $ model & fixedStep .~ True
            , Event $ AppRedistributePoints True
            , Event AppInit
            ]

applyFunction :: AppModel -> (Double, Double) -> (Double, Double)
applyFunction model p@(x, _) = newPoint where
    newPoint = if null cf
        then p
        else (x, fst (functions!!(fromJust cf)) x)
    cf = model ^. currentFunction

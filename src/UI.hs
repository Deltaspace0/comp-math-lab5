module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import Monomer.Graph
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack'
        [ case model ^. currentMenu of
            MGraph -> graphWithData_ points
                [ lockX_ $ model ^. xLock
                , lockY_ $ model ^. yLock
                , onRightClick AppAddPoint
                ] `nodeKey` "mainGraph"
            MInter -> interpolationPanel
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
            , hstack'
                [ label "x ="
                , numericField_ searchX
                    [ decimals 3
                    , onChange (const AppInit :: Double -> AppEvent)
                    ]
                , labeledCheckbox_ "Fixed step" fixedStep
                    [ onChange AppRedistributePoints
                    , onChange (const AppInit :: Bool -> AppEvent)
                    ] `nodeEnabled` (method == Lagrange)
                ]
            , hstack'
                [ label "h ="
                , numericFieldV currentStep AppStepChange
                ] `nodeVisible` (model ^. fixedStep)
            , separatorLine
            , hgrid'
                [ optionButton "Graph" MGraph currentMenu
                , optionButton "Interpolations" MInter currentMenu
                ]
            , separatorLine
            , hgrid'
                [ button "Interpolate" AppInterpolate
                    `nodeEnabled` (not $ model ^. instantInter)
                , labeledCheckbox_ "Instant" instantInter
                    [onChange (const AppInit :: Bool -> AppEvent)]
                ]
            , separatorLine
            , vstack'
                [ label "Add points with right mouse button"
                , button "Remove all points" AppRemovePoints
                , vscroll $ vstack' $
                    [ hgrid'
                        [ label "X:"
                        , label "Y:"
                        ]
                    ] <> pointPanels
                ]
            ] `styleBasic` [sizeReqW $ fixedSize 320]
        ] `styleBasic` [padding 16]
    interpolationPanel = vstack'
        [ hgrid'
            [ optionButton_ "Lagrange" Lagrange currentMethod
                [onChange AppMethodChange]
            , optionButton_ "Newton" Newton currentMethod
                [onChange AppMethodChange]
            , optionButton_ "Gauss" Gauss currentMethod
                [onChange AppMethodChange]
            ]
        , separatorLine
        , case method of
            Lagrange -> vstack'
                [ label $ textPolynomial $ model ^. interPolynomial
                , label $ textSolution $ model ^. searchSolution
                ]
            Newton -> vstack'
                [ label $ textSolution $ model ^. searchSolution
                , separatorLine
                , differencesPanel
                ]
            Gauss -> vstack'
                [ label $ textSolution $ model ^. searchSolution
                , separatorLine
                , differencesPanel
                ]
        ] `styleBasic` [sizeReqW $ expandSize 100 1]
    differencesPanel = scroll $ hstack' $
        [ vstack'' $ [label "i"] <> (nfield' <$> [0..(length ps-1)])
        , separatorLine
        , vstack'' $ [label "x_i"] <> (nfield . fst <$> ps)
        , vstack'' $ [label "y_i"] <> (nfield . snd <$> ps)
        ] <> (columnDelta <$> [1..(length fds-1)])
    nfield v = numericFieldD_ (WidgetValue v) [decimals 4]
    nfield' v = numericFieldD_ (WidgetValue v) []
    labelDelta i = let p = if i == 1 then "" else showt i in
        label $ "Δ" <> p <> " y_i"
    columnDelta i = vstack'' $ [labelDelta i] <> (nfield <$> fds!!i)
    vstack'' ws = vstack' ws `styleBasic` [sizeReqW $ fixedSize 80]
    points =
        [
            [ graphPoints ps
            , graphColor black
            , graphSeparate
            , graphOnChange AppPointChange
            , graphOnClick AppPointClicked
            ]
        , if null (model ^. currentFunction)
            then []
            else
                [ graphPoints $ (\x -> (x, cf x)) <$> xs
                , graphColor brown
                ]
        ,   [ graphPoints $ model ^. interGraph
            , graphColor interpolationColor
            ]
        ,   [ graphPoint (sx, model ^. searchSolution)
            , graphColor interpolationColor
            , graphSeparate
            , graphOnChange $ const AppSearchXChange
            ]
        ]
    interpolationColor = case method of
        Lagrange -> orange
        Newton -> green
        Gauss -> blue
    pointPanels = makePointPanel <$> [0..length ps-1]
    makePointPanel i = hgrid'
        [ numericField_ (pointField i . _1)
            [ decimals 4
            , onChange $ \x -> AppPointChange i (x, psy!!i)
            ]
        , hstack'
            [ numericField_ (pointField i . _2)
                [ decimals 4
                , readOnly_ $ not $ null $ model ^. currentFunction
                , onChange $ (const AppInit :: Double -> AppEvent)
                ] `nodeKey` (showt i)
            , button "x" $ AppRemovePoint i
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
    sx = model ^. searchX
    xs = [-15, (-14.95)..15]
    ps = model ^. dataPoints
    fds = model ^. forwardDifferences
    method = model ^. currentMethod
    (psx, psy) = unzip ps
    currentStep = if length psx > 1
        then psx!!1 - psx!!0
        else 0
    vstack' = vstack_ [childSpacing_ 16]
    hstack' = hstack_ [childSpacing_ 16]
    hgrid' = hgrid_ [childSpacing_ 16]

textPolynomial :: [Double] -> Text
textPolynomial cs = result where
    result = if null cs
        then "No solution"
        else "f(x) = " <> nom <> (px $ fst $ head ics) <> rest
    nom = numericToText 5 $ snd $ head ics
    ics = reverse $ zip [0..] cs
    rest = mconcat $ f <$> tail ics
    f (i, x) = (formatNumber x) <> px i
    px :: Int -> Text
    px i
        | i == 0 = ""
        | i == 1 = "x "
        | otherwise = "x^" <> (showt i) <> " "
    formatNumber x = sign <> (numericToText 5 $ abs x) where
        sign = if x >= 0 then "+ " else "- "

textSolution :: Double -> Text
textSolution y = "y = " <> numericToText 5 y

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
                , numericField_ searchX [decimals 3]
                , labeledCheckbox_ "Fixed step" fixedStep
                    [onChange AppRedistributePoints]
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
            [ optionButton "Lagrange" Lagrange currentMethod
            , optionButton "Newton" Newton currentMethod
            , optionButton "Gauss" Gauss currentMethod
            ]
        , separatorLine
        , case model ^. currentMethod of
            Lagrange -> vstack'
                [ label $ textPolynomial $ model ^. interPolynomial
                , label $ textSolution $ interF sx
                ]
            Newton -> vstack'
                [ label $ textPolynomial $ model ^. interPolynomial
                , label $ textSolution $ interF sx
                ]
            Gauss -> vstack'
                [ label $ textPolynomial $ model ^. interPolynomial
                , label $ textSolution $ interF sx
                ]
        ] `styleBasic` [sizeReqW $ expandSize 100 1]
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
        ,   [ graphPoints $ (\x -> (x, interF x)) <$> xs
            , graphColor $ case model ^. currentMethod of
                Lagrange -> orange
                Newton -> green
                Gauss -> blue
            ]
        ]
    pointPanels = makePointPanel <$> [0..length ps-1]
    makePointPanel i = hgrid'
        [ numericField_ (pointField i . _1)
            [ decimals 3
            , onChange $ \x -> AppPointChange i (x, psy!!i)
            ]
        , hstack'
            [ numericField_ (pointField i . _2)
                [ decimals 3
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
    interF = getInterFunction $ model ^. interPolynomial
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

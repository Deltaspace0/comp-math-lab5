{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( Menu(..)
    , AppModel(..)
    , xLock
    , yLock
    , dataPoints
    , currentFunction
    , searchX
    , fixedStep
    , currentMenu
    , initModel
    , functions
    ) where

import Control.Lens
import Data.Text (Text)

data Menu
    = MGraph
    | MInter
    deriving (Eq, Show)

data AppModel = AppModel
    { _amXLock :: Bool
    , _amYLock :: Bool
    , _amDataPoints :: [(Double, Double)]
    , _amCurrentFunction :: Maybe Int
    , _amSearchX :: Double
    , _amFixedStep :: Bool
    , _amCurrentMenu :: Menu
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amXLock = False
    , _amYLock = False
    , _amDataPoints = []
    , _amCurrentFunction = Nothing
    , _amSearchX = 0
    , _amFixedStep = False
    , _amCurrentMenu = MGraph
    }

functions :: [(Double -> Double, Text)]
functions =
    [ (\x -> sin x, "f(x) = sin(x)")
    , (\x -> (sin $ x**2), "f(x) = sin(x^2)")
    , (\x -> exp (-x**2), "f(x) = e^(-x^2)")
    , (\x -> (sin x)/x, "f(x) = sin(x)/x")
    , (\x -> x, "f(x) = x")
    , (\x -> (log x)*(cos x), "f(x) = ln(x)cos(x)" )
    , (\x -> 1/x, "f(x) = 1/x")
    ]

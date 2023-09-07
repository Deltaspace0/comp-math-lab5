{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( AppModel(..)
    , xLock
    , yLock
    , dataPoints
    , initModel
    ) where

import Control.Lens

data AppModel = AppModel
    { _amXLock :: Bool
    , _amYLock :: Bool
    , _amDataPoints :: [(Double, Double)]
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amXLock = False
    , _amYLock = False
    , _amDataPoints = []
    }

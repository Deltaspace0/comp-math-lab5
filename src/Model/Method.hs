module Model.Method
    ( Method(..)
    , getInterFunction
    ) where

data Method
    = Lagrange
    | Newton
    | Gauss
    deriving (Eq, Show)

getInterFunction :: [Double] -> (Double -> Double)
getInterFunction [] _ = 0
getInterFunction cs x = sum $ zipWith (*) cs $ (x**) <$> [0..]

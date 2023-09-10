module Model.Method
    ( Method(..)
    , getInterFunction
    , interpolateLagrange
    ) where

import Data.Matrix hiding (toList)
import Data.Vector (toList)

data Method
    = Lagrange
    | Newton
    | Gauss
    deriving (Eq, Show)

getInterFunction :: [Double] -> (Double -> Double)
getInterFunction [] _ = 0
getInterFunction cs x = sum $ zipWith (*) cs $ (x**) <$> [0..]

interpolateLagrange :: [(Double, Double)] -> [Double]
interpolateLagrange dp = getCoefs dp fL where
    fL sx = sum $ zipWith f ixs dp where
        f i (x, y) = y*(g i x $ zip ixs dp)
        g _ _ [] = 1
        g i x ((j, (xj, _)):xs) = if i == j
            then g i x xs
            else (sx-xj)/(x-xj)*(g i x xs)
    ixs = [0..] :: [Int]

getCoefs
    :: [(Double, Double)]
    -> (Double -> Double)
    -> [Double]
getCoefs ps f = result where
    result = if null ps
        then []
        else case rref vandermonde of
            Left _ -> []
            Right mat -> toList $ getCol (ncols mat) mat
    vandermonde = fromLists $ makeRow <$> ps
    makeRow (x, _) = ((\i -> x**i) <$> [0..n]) <> [f x]
    n = fromIntegral $ length ps - 1

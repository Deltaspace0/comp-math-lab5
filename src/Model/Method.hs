module Model.Method
    ( Method(..)
    , getInterFunction
    , getDifferences
    , interpolateLagrange
    , interpolateNewton
    ) where

import Data.Matrix hiding (toList)
import Data.Vector (toList)
import GHC.Utils.Misc (dropTail)

data Method
    = Lagrange
    | Newton
    | Gauss
    deriving (Eq, Show)

getInterFunction :: [Double] -> (Double -> Double)
getInterFunction [] _ = 0
getInterFunction cs x = sum $ zipWith (*) cs $ (x**) <$> [0..]

getDifferences :: [(Double, Double)] -> [[Double]]
getDifferences dp = take (length dp) differences where
    differences = iterate generateDifferences $ snd <$> dp
    generateDifferences ys = zipWith (-) (tail ys) ys

interpolateLagrange :: [(Double, Double)] -> [Double]
interpolateLagrange dp = getCoefs dp fL where
    fL sx = sum $ zipWith f ixs dp where
        f i (x, y) = y*(g i x $ zip ixs dp)
        g _ _ [] = 1
        g i x ((j, (xj, _)):xs) = if i == j
            then g i x xs
            else (sx-xj)/(x-xj)*(g i x xs)
    ixs = [0..] :: [Int]

interpolateNewton
    :: [(Double, Double)]
    -> [[Double]]
    -> Double
    -> [Double]
interpolateNewton [] _ _ = []
interpolateNewton (_:[]) _ _ = []
interpolateNewton dp differences sx = getCoefs dp' fN where
    leftHalf = sx <= (fst (head dp) + fst (last dp))/2
    dp' = if leftHalf
        then dropWhile (\(x, _) -> x < sx) dp
        else takeWhile (\(x, _) -> x <= sx) dp
    h = (fst (dp'!!1) - fst (dp'!!0))
    n = length dp'
    dl = length dp - n
    dys = if leftHalf
        then head <$> (drop dl <$> differences)
        else last <$> (dropTail dl <$> differences)
    fN x = result where
        result = sum $ zipWith (\dy i -> dy*(ft i)) dys [0..(n-1)]
        ft 0 = 1
        ft i = if leftHalf
            then (product $ (t-) <$> [0..(i'-1)])/fact
            else (product $ (t+) <$> [0..(i'-1)])/fact
            where
                fact = product [1..i']
                i' = fromIntegral i
        t = if leftHalf
            then (x-(fst (head dp')))/h
            else (x-(fst (last dp')))/h

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

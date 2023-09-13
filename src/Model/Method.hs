module Model.Method
    ( Method(..)
    , getInterFunction
    , getDifferences
    , interpolateLagrange
    , interpolateNewton
    , interpolateGauss
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
    -> Double
interpolateNewton [] _ _ = 0
interpolateNewton (_:[]) _ _ = 0
interpolateNewton dp differences sx = result where
    result = sum $ zipWith (\dy i -> dy*(ft i)) dys [0..(n-1)]
    ft 0 = 1
    ft i = if leftHalf
        then (product $ (t-) <$> [0..(i'-1)])/fact
        else (product $ (t+) <$> [0..(i'-1)])/fact
        where
            fact = product [1..i']
            i' = fromIntegral i
    t = if leftHalf
        then (sx-(fst (head dp')))/h
        else (sx-(fst (last dp')))/h
    h = (fst (dp'!!1) - fst (dp'!!0))
    dys = if leftHalf
        then head . drop dl <$> differences
        else last . dropTail dl <$> differences
    leftHalf = sx <= (fst (head dp) + fst (last dp))/2
    dl = length dp - n
    n = length dp'
    dp' = if leftHalf
        then (if null twdp then [] else [last twdp]) <> dwdp
        else twdp <> (take 1 dwdp)
    twdp = takeWhile (\(x, _) -> x < sx) dp
    dwdp = dropWhile (\(x, _) -> x < sx) dp

interpolateGauss
    :: [(Double, Double)]
    -> [[Double]]
    -> Double
    -> Double
interpolateGauss [] _ _ = 0
interpolateGauss (_:[]) _ _ = 0
interpolateGauss dp differences sx = result where
    result = if sx >= (fst (head dp) + fst (last dp))/2
        then interpolateGaussForward dp differences sx
        else interpolateGaussBackward dp differences sx

interpolateGaussForward
    :: [(Double, Double)]
    -> [[Double]]
    -> Double
    -> Double
interpolateGaussForward dp differences sx = result where
    result = sum $ zipWith (\dy i -> dy*(ft i)) dys [0..(2*nd)]
    ft 0 = 1
    ft i = (product $ (t+) <$> ins)/(product [1..i']) where
        ins = take i $ 0:([1..] >>= \x -> [-x, x])
        i' = fromIntegral i
    t = (sx-(fst $ dp!!nd))/h
    h = (fst (dp!!1) - fst (dp!!0))
    dys = (\(a, b) -> a!!b) <$> ddd
    ddd = takeWhile lengthCheck $ zip differences diffIndices
    lengthCheck (a, b) = b < length a
    diffIndices = [nd, (nd-1)..] >>= \x -> [x, x]
    nd = -1+(length $ takeWhile (\(x, _) -> x < sx) dp)

interpolateGaussBackward
    :: [(Double, Double)]
    -> [[Double]]
    -> Double
    -> Double
interpolateGaussBackward dp differences sx = result where
    result = sum $ zipWith (\dy i -> dy*(ft i)) dys [0..(2*nd)]
    ft 0 = 1
    ft i = (product $ (t+) <$> ins)/(product [1..i']) where
        ins = take i $ 0:([1..] >>= \x -> [x, -x])
        i' = fromIntegral i
    t = (sx-(fst $ dp!!nd))/h
    h = (fst (dp!!1) - fst (dp!!0))
    dys = (\(a, b) -> a!!b) <$> ddd
    ddd = takeWhile lengthCheck $ zip differences diffIndices
    lengthCheck (a, b) = b < length a
    diffIndices = tail $ [nd, (nd-1)..] >>= \x -> [x, x]
    nd = length $ takeWhile (\(x, _) -> x < sx) dp

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

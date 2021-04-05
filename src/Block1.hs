module Block1
    ( f1_1
    , f2_1
    , f3_1
    , f4_1
    , f5_1
    , f6_1
    , f7_1
    , f8_1
    , f10_1
    , f11_1
    , f12_1
    ) where

import GHC.Float ( double2Int, int2Double )

-- return given number
f1_1 :: Int -> Int
f1_1 = id

-- check if given number is prime
f2_1 :: Int -> Bool
f2_1 n
  | n == 0 || abs n == 1 = False
  | otherwise = all (\m -> nAbs `mod` m /= 0) [2..nAbs-1] where nAbs = abs n

-- return sum of 2 bools as int
f3_1 :: Bool -> Bool -> Int
f3_1 n m = toInt n + toInt m
  where
    toInt :: Bool -> Int
    toInt False = 0
    toInt True = 1

-- return sum of number's divisors (without number itself)
f4_1 :: Int -> Int
f4_1 n = foldl (\acc m -> if nAbs `mod` m == 0 then acc + m else acc) 0 [1..nAbs-1]
  where nAbs = abs n

-- return next perfect number
f5_1 :: Int -> Int
f5_1 n
  | n <= 0 = 6
  | otherwise = go $ n + 1 
  where
    go :: Int -> Int
    go m = if f4_1 m == m then m else go $ m + 1

-- ^ the same as f5_1 but for `Integer` type
f6_1 :: Integer -> Integer
f6_1 n = toInteger $ f5_1 $ fromIntegral n

-- return Ackerman function result
f7_1 :: Int -> Int -> Int
f7_1 m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = f7_1 (m - 1) 1
  | m > 0 && n > 0 = f7_1 (m - 1) (f7_1 m (n - 1))

-- return Ackerman function result
-- as `Integer` type
f8_1 :: Int -> Int -> Integer
f8_1 n = toInteger . f7_1 n

-- return integer part and remainder
-- after dividing 2 double numbers 
f10_1 :: Double -> Double -> (Double, Double)
f10_1 a b = (integerPart, remainder)
  where
    integerPart = int2Double $ double2Int $ a / b
    remainder = a - b * integerPart

-- return result of first k steps of
-- continued fraction accroding to Brouncker formula
f11_1 :: Int -> Double
f11_1 k = 1.0 / (1 + 1 / foldr reducer 2.0 [3, 5 .. k*2-1])
  where
    reducer :: Int -> Double -> Double
    reducer value acc = 2 + (int2Double (value ^ 2) / acc)

-- return expression `e`
-- such that `e == 1 + 1` is `True`
f12_1 :: Int
f12_1 = 1 + 1

-- Done: 11/12
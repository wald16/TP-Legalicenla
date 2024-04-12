-- a)
absoluto :: Integer -> Integer
absoluto x | x < 0 = -x
             | otherwise = x

-- b)
maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y | absoluto x  > absoluto y = absoluto x
                   | otherwise              = absoluto y

-- c)
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y
              | otherwise  = z

-- d)
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x == 0 || y == 0 = True
              | otherwise        = False

algunoEs0_ :: Float -> Float -> Bool
algunoEs0_ 0 _  = True
algunoEs0_ _ 0   = True
algunoEs0_ _ _ = False


-- e)   
ambosSon0 ::  Float -> Float -> Bool
ambosSon0 x y = x == 0 && y == 0 

ambosSon0_  :: Float -> Float -> Bool
ambosSon0_ 0 0 = True
ambosSon0_ _ _ = False

-- f)
mismoIntervalo  :: Float -> Float -> Bool
mismoIntervalo x y | (x <= 3 && y <= 3) || (x > 3 && x <= 7 && y > 3 && y <= 7) || (x > 7 && y >= 5) = True
                   | otherwise = False

-- g)
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos a b c | a  /= b && a /= c && b /= c = a + b + c
                    | a == b && a /= c = c
                    | a /= b && b == c = a
                    | otherwise = b

-- h)
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe n m | mod  n m == 0 || mod m n == 0 = True
                 | otherwise     = False

-- i)
digitoUnidades :: Integer -> Integer
digitoUnidades  n | n >= 0 = mod n 10
                  | otherwise  = abs  n `mod` 10

-- j)
digitoDecenas :: Integer -> Integer
digitoDecenas n | n >= 0 = digitoUnidades (div n 10)
                | n < 0 = digitoUnidades (div (-n) 10)
                  
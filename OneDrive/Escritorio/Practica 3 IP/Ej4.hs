

-- EJ 1
todoMenor :: (Float,Float) -> (Float,Float) -> Bool

todoMenor (t11, t12) (t21, t22) = t11 < t21 && t12 < t22

-- EJ 2
posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (x, y, z)  |  even x = 1
                        |  even y = 2
                        |  even z = 3 
                        |  otherwise = 4



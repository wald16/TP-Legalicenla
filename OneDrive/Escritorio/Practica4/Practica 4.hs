
esPar :: Int -> Bool
esPar n | n==0 = True
        | otherwise = not (esPar (n-1))


--Ej 1
fibonacci:: Integer ->Integer
fibonacci n | n < 0 = error "Debe ser mayor o igual a 0"
            | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci (n -1) + fibonacci(n -2)
--Ej 2
parteEntera :: Float -> Integer
parteEntera x | x < 0 = (-1) * (parteEntera' (abs x) res)
              |  otherwise = parteEntera' x res
                 where res = 0 :: Integer
                       parteEntera' :: Float -> Integer  -> Integer
                       parteEntera' x res | fromInteger res <= x && x < fromInteger (res+1) = res
                                          | otherwise = parteEntera' x (res + 1)

--Ej3
esDivisible :: Integer -> Integer -> Bool
esDivisible x y | x < y = False
                | x == y = True
                | otherwise = esDivisible (x - y) y

--Ej4
sumaImpares :: Integer -> Integer 
sumaImpares 1 = 1
sumaImpares x = (2*x - 1) + sumaImpares (x - 1)


--Ej5
medioFact :: Integer -> Integer 
medioFact n  | n < 0  = error "Debe ser un numero positivo"
medioFact n = medioFact' n
     where medioFact' :: Integer -> Integer
           medioFact' n  | n <= 0 = 1  
                         | otherwise = n * medioFact' (n-2)

--Ej6
sumaDigitos :: Integer -> Integer
sumaDigitos x = sumaD x
 where sumaD :: Integer -> Integer
       sumaD x | div x  10 < 1  = x
               | otherwise   = sumaD (div x 10) +  mod x 10

--Ej7
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales x  | x <= 0 = error  "No puede ser cero o negativo"
                       | otherwise = todosDII x
                             where todosDII :: Integer -> Bool
                                   todosDII x | div x 10 == 0 = True
                                              | mod x 10 /= mod(div x 10) 10 = False 
                                              | otherwise = todosDII(div x 10)

-- Ej8 
-- problema iesimoDigito (n: Z, i: N) : Z {
-- requiere: { n ≥ 0 ∧ 1 ≤ i ≤ cantDigitos(n) }
-- asegura: { resultado = (n div 10cantDigitos(n)−i
-- ) mod 10 }
-- }

-- problema cantDigitos (n: Z) : N {
-- requiere: { n ≥ 0 }
-- asegura: { n = 0 → resultado = 1}
-- asegura: { n ̸= 0 → (n div 10resultado−1 > 0 ∧ n div 10resultado = 0) }
-- }


cantDigitos :: Integer -> Integer
cantDigitos n    | n < 0 = error  "debe ser mayor o igual a 0"
                 | n == 0 = 1
                 | otherwise = cantDigitos' n 
                               where cantDigitos' :: Integer -> Integer
                                     cantDigitos' x | div  x 10 == 0 = 1
                                                    | otherwise = 1 + cantDigitos' (div x 10) 
                               
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i | n < 0 || i < 1 || i > cantDigitos n = error "Error en los parametros"
                 | otherwise = mod (div n (10 ^ (fromInteger (cantDigitos n) - i))) 10

--Ej9
esCapicua :: Integer -> Bool 
esCapicua n | n < 0  = error "Debe ser positivo"
esCapicua n = n == esCapicua' n 0 
                          where  esCapicua' :: Integer -> Integer -> Integer
                                 esCapicua' 0 reversed = reversed
                                 esCapicua' num reversed = esCapicua' (num `div` 10) (reversed * 10 + mod num 10)
                                 
--Ej10                                     
--a                                       
f1 :: Integer -> Integer
f1 0 = 1
f1 n = 2 ^ n + f1 (n-1)

--b
f2 :: Integer -> Float -> Float
f2 1 q = q
f2 n q = (q ^ fromInteger n) + f2 (n-1) q

--c 
f3 :: Integer -> Float -> Float
f3 1 q = q
f3 n q = (q ^ fromInteger (2 * n)) + f2 (2 * n - 1) q

--d
f4 :: Integer -> Float -> Float
f4 n q = f4' n q elev
      where elev = 2 * n :: Integer
            f4' :: Integer -> Float -> Integer -> Float
            f4' n q elev    | n == elev = (q ^ fromInteger n)
                            | otherwise = (q ^ fromInteger elev) + f4' n q (elev - 1)
               
--Ej11  a
eAprox :: Integer -> Float 
eAprox 0 = 1
eAprox n = 1 / eAprox' n + eAprox(n - 1)
      where 
            eAprox' :: Integer -> Float
            eAprox' 0 = 1
            eAprox' h |  h < 0 = error "Debe ser mayor o igual a 0"
                      | otherwise = fromIntegral h * eAprox'(h-1) 

-- mas facil si defino factorial primero
factorial ::  Integer -> Integer  
factorial n = factorial' n 1
     where  
          factorial' :: Integer -> Integer -> Integer  
          factorial' 0 fact = fact    
          factorial' d fact = factorial' (d-1) (fact*d) 

e :: Float
e = eAprox 10

--Ej12
raizDe2Aprox :: Integer -> Float
raizDe2Aprox n | n <= 0 = error  "El numero debe de ser mayor que cero"
raizDe2Aprox 1 = 2
raizDe2Aprox n = 2 + 1 / raizDe2Aprox (n - 1)

--Ej13 DUDA RESUELTA
sumaDeSuma :: Integer -> Integer -> Integer
sumaDeSuma n m | n < 1 || m < 1 = error  "debe ser mayor a 1"
sumaDeSuma 1 m = 1
sumaDeSuma n m = sumaDePotencia n m + sumaDeSuma(n - 1) m

sumaDePotencia :: Integer -> Integer -> Integer
sumaDePotencia n 1 = n
sumaDePotencia n j = n ^ j + sumaDePotencia n (j-1)

--Ej14 
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m | q <= 0 || n  <= 0 || m <= 0 = error "Los numeros tienen que ser mayores a cero"
sumaPotencias q n m = q ^ ((todosN n) + (todosN m)) 

todosN :: Integer -> Integer
todosN 1 = 1
todosN x = x + todosN(x-1)

--Ej15
sumaRacionales :: Integer ->Integer ->Float
sumaRacionales n m | n <= 0 || m <= 0 = error "deben ser naturales"
sumaRacionales n 1 = n
sumaRacionales n m = todosN n  div todosN m

division :: Integer -> Integer -> Integer
division _ 0  = error "No se puede dividir entre cero"
division p q | 






 
                        

            


    
    
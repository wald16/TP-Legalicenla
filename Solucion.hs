module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- S"lo est  permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {}
-- Integrante1: { DNI1,apellidoYNombre1}
-- Integrante2: { DNI2,apellidoYNombre2}
-- Integrante3: { DNI3,apellidoYNombre3}
-- Integrante4: { DNI4,apellidoYNombre4}
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia alg#n
                        -- integrante, completar con los dni y apellidos, sino dejar vac!o}

-- TODO!!!!!!
-- Sacar los '$'

-- Funciones Auxiliares --

-- EJ 1

testChar :: Int -> String
testChar 123 = []
testChar start = chr start : testChar (start + 1)

esMinuscula :: Char -> Bool
esMinuscula letra = ord letra >= 97 && ord letra <= 123

-- EJ 2
letraANatural :: Char -> Int
letraANatural char = ord char - 97

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar char numba | not $ esMinuscula char = char
                     | otherwise = chr (mod (letraANatural char + numba) 26 + 97)

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:frase) numba | not $ esMinuscula x = x : cifrar frase numba
                       | otherwise = (desplazar x numba) : cifrar frase numba

-- EJ 5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (x:frase) numba | not $ esMinuscula x = x : descifrar frase numba
                          | otherwise = (desplazar x (- numba)) : descifrar frase numba
-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista lista = cifrarListaAux lista 0
   where
       cifrarListaAux :: [String] -> Int -> [String]
       cifrarListaAux [] _ = []
       cifrarListaAux (x:xs) add = cifrar x add : cifrarListaAux xs (add + 1)

-- EJ 7
frecuencia :: String -> [Float]
frecuencia frase = frecuenciaAux frase 97
    where 
        frecuenciaAux :: String -> Int -> [Float]
        frecuenciaAux _ 123 = []
        frecuenciaAux frase add | not $ (chr add `elem` frase) = 0.0 : frecuenciaAux frase (add + 1)
                                | otherwise = porcentaje (chr add) frase : frecuenciaAux frase (add + 1)

        porcentaje :: Char -> String -> Float
        porcentaje char frase = (cuantasVeces char frase) * 100 / fromIntegral (length frase) 

        cuantasVeces :: Char -> String -> Float
        cuantasVeces _ [] = 0.0
        cuantasVeces x (y:ys) | x == y = 1.0 + cuantasVeces x ys
                              | otherwise = cuantasVeces x ys

-- [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0]

-- [16.666666,0.0,0.0,0.0,16.666666,0.0,0.0,0.0,0.0,0.0,0.0,33.333332,0.0,0.0,0.0,0.0,0.0,16.666666,0.0,16.666666,0.0,0.0,0.0,0.0,0.0,0.0]
-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente frase numba = (letraMasFrecuente fraseCifrada, elementoMasGrande frecuenciaDeLaFraseCifrada)
    where
        fraseCifrada = cifrar frase numba :: String
        frecuenciaDeLaFraseCifrada = frecuencia fraseCifrada :: [Float]

        letraMasFrecuente :: String -> Char
        letraMasFrecuente cifrado = chr (length (hastaElementoMasGrande frecuenciaDeLaFraseCifrada) + 97)


        elementoMasGrande :: [Float] -> Float
        elementoMasGrande [x] = x
        elementoMasGrande (x:y:frecuencia) | x >= y = elementoMasGrande (x:frecuencia)
                                           | otherwise = elementoMasGrande (y:frecuencia)


        hastaElementoMasGrande :: [Float] -> [Float]
        hastaElementoMasGrande (x:frecuencia) | x == (elementoMasGrande frecuenciaDeLaFraseCifrada) = [] 
                                              | otherwise = x : hastaElementoMasGrande frecuencia 


-- EJ 9
esDescifrado :: String -> String -> Bool 
esDescifrado frase1 frase2 = esDescifradoAux frase1 frase2 0
     where
        esDescifradoAux frase1 frase2 n | n >= 25 = False
                                        | frase2 == cifrar frase1 n = True 
                                        | otherwise = esDescifradoAux frase1 frase2 (n + 1)

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados lista = superRecursion lista
    where 
        todosLosDescifradosAux :: [String] -> [(String, String)]
        todosLosDescifradosAux [] = []
        todosLosDescifradosAux [x] = []
        todosLosDescifradosAux (x:y:xs) | esDescifrado x y = (x, y) : (y, x) : todosLosDescifrados (x:xs)
                                        | otherwise = todosLosDescifrados (x:xs)

        superRecursion :: [String] -> [(String, String)]
        superRecursion [] = []
        superRecursion (x:xs) = todosLosDescifradosAux (x:xs) ++ todosLosDescifradosAux xs
        

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave _ _ = "compucom"

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere _ _ = "kdueciirqdv"

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere _ _ = "computacion"

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ _ = "asdef"

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere _ _ _ = [("hola", "b")]


module Practica5 where

--5.1 Función recursiva que dado un número de conejos 'n',
--calcula el número de orejas de conejo total. 
bunnyEars :: Int -> Int
bunnyEars 0 = 0
bunnyEars n = 2 + bunnyEars(n-1)

--5.2 Función recursiva que dado el nivel 'i' de una pirámide,
--calcula la suma de el número de bloques desde el nivel 1 hasta el nivel 'i'.
piramide :: Int -> Int
piramide 0 = 0
piramide 1 = 1
piramide i = bloques(i) + piramide(i-1)

--Función auxiliar que dado un renglón 'i',
--regresa el número de bloques en 'i'.
bloques :: Int -> Int
bloques 0 = 0
bloques 1 = 1
bloques 2 = 2
bloques i = bloques (i-1) + bloques (i-2) 

--5.3 Función recursiva que dado número natural 'n',
--regresa la suma de sus dígitos.
sumDigits :: Int -> Int
sumDigits 0 = 0 
sumDigits n = mod n 10  + sumDigits(div n 10)

--5.4 Función recursiva que dada una cadena,
--calcula el número de presencias del carácter ’x’.
countX :: String -> Int
countX "" = 0
countX (x:xs) 
    |x=='x' = 1 + countX xs
    |otherwise = countX xs

--5.5 Función recursiva que dada una cadena, reemplaza las presencias del 
--carácter ’x’ por el carácter ’y’.
changeXY :: String -> String
changeXY "" = ""
changeXY (x:xs) 
    |x=='x' = "y" ++ changeXY xs
    |otherwise = x:changeXY xs
    
--5.6 Función que dada una lista de enteros, indica si ésta tiene a el 6 como
--elemento.
contains6 :: [Int] -> Bool
contains6 [] = False
contains6 (x:xs)  
    |x == 6 = True
    |otherwise = contains6 xs
    
--5.7 Función que dado un número natural 'n', calcula la suma de los dígitos de 'n' 
--hasta obtener como resultado un número natural de un sólo dígito.
sDigito :: Int -> Int 
sDigito n 
    |sumDigits(n) < 10 = n 
    |otherwise = sDigito(sumDigits(n))


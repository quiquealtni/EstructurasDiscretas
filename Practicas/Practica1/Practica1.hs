module Practica1 where

{-Ejercicio 1.1 
Función que dados 2 números enteros, devuelve el resultado de multiplicarlos -}
multiplica :: Int -> Int -> Int
multiplica x y = x * y

{- Ejercicio 1.2 
Función que dados 2  números flotantes, devuelve el resultado de dividir 
el primero entre el segundo -}
divide :: Float -> Float -> Float
divide x y =
   if y==0 then
   error "No puedes dividir entre 0"
   else
   x/y

{- Ejercicio 1.3 
Función que dados 2 números enteros, devuelve el resultado de la suma de
el primer numero elevado al segundo y viceversa, es decir:
"numero1^numero2 + numero2^numero1" -}
potSum:: Int -> Int -> Int
potSum x y = x^y + y^x

{- Ejercicio 1.4 
Función que dados 2 números enteros,tal que:si el primero es mayor que el segundo,
los multiplica, si el segundo es mayor que el primero, los suma y si son iguales
los divide.-}
multSumDiv :: Int -> Int -> Int
multSumDiv x y
    |x > y = x * y
    |x == y = div x y
    |otherwise = x + y

{- Ejercicio 1.5 
Función que dada una operación (representada por un caracter) y dados 2 numeros 
enteros,devuelve el resultado de aplicar la operación a los 2 enteros dados -}
calcula :: Char -> Int -> Int -> Int
calcula c x y
    |c == 's' = x 
    |c == 't' = y
    |c == 'a' = x + y 
    |c == 'r' = x - y
    |c == 'p' = x * y
    |c == 'd' = div x y
    |c == 'e' = x^y
    |otherwise = error "Operacion no valida"
    
{- Ejercicio 1.6
Función que dados los coeficientes de una ecuación cuadrática, devuelve las ráices
de esta -}
raizEcCuadr :: Float -> Float -> Float -> (Float , Float)
raizEcCuadr a b c = ( ((-1)*b + raiz) / (2*a) , ((-1)*b - raiz) / (2*a) )
    where raiz = sqrt((b^2) - (4*a*c)) 
          
{- Ejercicio 1.7
Función que dados el radio y el centro (h,k) de una circunferencia, 
y además un punto (x,y),verifica si el punto está o no en la 
circunferencia dada -}
enCircunferencia :: Float -> (Float , Float) -> (Float , Float) -> Bool
enCircunferencia r (h,k) (x,y)= punto == r^2
    where punto = (x - h)^2 + (y - k)^2 

{- Ejercicio 1.8 
Función que recibe una lista de enteros en el rango de 1 a 12 y regresa 
una nueva lista con los meses que representan los enteros de la lista original. -}
mes :: Int -> [String]
mes n
   | n == 1 = ["enero"]
   | n == 2 = ["febrero"]
   | n == 3 = ["marzo"]
   | n == 4 = ["abril"]
   | n == 5 = ["mayo"]
   | n == 6 = ["junio"]
   | n == 7 = ["julio"]
   | n == 8 = ["agosto"]
   | n == 9 = ["septiembre"]
   | n == 10 = ["octubre"]
   | n == 11 = ["noviembre"]
   | n == 12 = ["diciembre"]
   | otherwise = error "No es un mes."

meses :: [Int] -> [String]
meses [] = []
meses (x:xs)
    |x < 0 || x > 12 = meses xs
    |otherwise = mes x ++ meses xs
       
{- Ejercicio 1.9
Función que dados el radio y el centro (h,k) de una circunferencia,
y además una lista de puntos (x,y), verifica si cada uno de los puntos
de la lista dada se encuentra o no en la circunferencia y devuelve
una nueva lista con los puntos que sí estań en la circunferencia -}
puntosEnCircunferencia :: Float -> (Float , Float ) -> [( Float , Float )] -> [( Float , Float )]
puntosEnCircunferencia r c lp = filter puntos lp
    where puntos (x,y) = enCircunferencia r c (x,y)       
          
          
{- Ejercicio 1.10
Representar mediante listas por comprensión los siguientes conjuntos:
los pares del 2 al 20 , los multiplos de cinco del 5 al 50 y las
potencias de 2^1 hasta 2^8 -}
pares = [2*x | x <- [1..10]]
multiplosCinco = [5*x | x <- [1..10]]
potencias = [2^x | x <- [1..8]]

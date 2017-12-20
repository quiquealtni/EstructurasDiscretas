module Practica6 where

data Natural = Cero
             | Suc Natural deriving(Eq,Show)
             
data ArbolBH a = Hoja a
               | Nodo (ArbolBH a) (ArbolBH a) deriving(Eq,Show)

-- Funcion que suma dos naturales.               
suma :: Natural -> Natural -> Natural
suma m Cero = m
suma m (Suc n) = Suc(suma m n)

-- Funcion que multiplica dos naturales.
multiplica :: Natural -> Natural -> Natural
multiplica _ Cero = Cero
multiplica m (Suc n) = suma m (multiplica m n)

-- Funcion que convierte un numero natural a entero.
aEntero :: Natural -> Int
aEntero Cero = 0
aEntero (Suc n) = 1 + aEntero n

-- Funcion que convierte un entero a un numero natural.
aNatural :: Int -> Natural
aNatural 0 = Cero
aNatural n = Suc (aNatural (n-1))

-- Funcion que regresa el numero de hojas de un ArbolBH.
numeroHojas :: ArbolBH a -> Int
numeroHojas (Hoja a) = 1
numeroHojas (Nodo a b) = numeroHojas a + numeroHojas b

-- Funcion que indica si un ArbolBH es balanceado.
esBalanceado :: ArbolBH a -> Bool
esBalanceado (Hoja a) = True
esBalanceado (Nodo a b)
    |(numeroHojas a > numeroHojas b + 1 ) || (numeroHojas b > numeroHojas a + 1)= False 
    |otherwise = esBalanceado a && esBalanceado b
    
construyeBalanceado :: [a] -> ArbolBH a
construyeBalanceado (x:xs) = error "Funcion no implementada"

-- Funcion que regresa la lista de los elementos de un arbol.
listaArbol :: ArbolBH a -> [a]
listaArbol (Hoja a) = [a]
listaArbol (Nodo a b) = listaArbol a ++ listaArbol b


               

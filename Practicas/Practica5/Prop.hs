module Prop (Booleano(Verdadero,Falso), Prop(Cte,Var,Neg,Conj,Disy,Impl,Syss), evalua, 
    esTautologia) where

import Data.List

-- Tipo de dato para representar a las constantes lógicas
-- verdadero y falso .
data Booleano = Verdadero
              | Falso deriving(Show)

-- Tipo de dato para representar expresiones de la
-- lógica proposicional
data Prop = Cte Booleano
          | Var String
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Impl Prop Prop
          | Syss Prop Prop

-- Permite mostrar las fórmulas proposicionales en un formato amigable
instance Show Prop where
   show p = showProp p

-- Función que permite implementar la representación "amigable" de una fórmula proposicional.
showProp :: Prop -> String
showProp (Cte a) = show a
showProp (Var a) = a
showProp (Neg a) = "-" ++ show a
showProp (Conj f s) = show f ++ " ^ " ++ show s
showProp (Disy f s) = "(" ++ show f ++ " v " ++ show s ++ ")"
showProp (Impl f s) = "(" ++ show f ++ " -> " ++ show s ++ ")"
showProp (Syss f s) = "(" ++ show f ++ " <-> " ++ show s ++ ")"

----------------------------------------------------------------------------------------------------
------                                                                                        ------
------                       Funciones de interpretación                                      ------
------                                                                                        ------
----------------------------------------------------------------------------------------------------

-- *. Negacion
negacion :: Booleano -> Booleano
negacion Verdadero = Falso
negacion Falso = Verdadero

-- 0. Constante binaria 0.
cte0 :: Booleano -> Booleano -> Booleano
cte0 _ _ = Falso

-- 1. Conjunción.
conjuncion :: Booleano -> Booleano -> Booleano
conjuncion Falso Falso = Falso
conjuncion Falso Verdadero = Falso 
conjuncion Verdadero Falso = Falso 
conjuncion Verdadero Verdadero = Verdadero 

-- 2. Inhibición (Izquierda).
inhibicionIzq :: Booleano -> Booleano -> Booleano
inhibicionIzq Falso Falso = Falso
inhibicionIzq Falso Verdadero = Falso 
inhibicionIzq Verdadero Falso = Verdadero 
inhibicionIzq Verdadero Verdadero = Falso 

-- 3. Transferencia (Izquierda).
transfIzq :: Booleano -> Booleano -> Booleano
transfIzq Falso Falso = Falso
transfIzq Falso Verdadero = Falso 
transfIzq Verdadero Falso = Verdadero 
transfIzq Verdadero Verdadero = Verdadero 

-- 4. Inhibición (Derecha).
inhibicionDer :: Booleano -> Booleano -> Booleano
inhibicionDer Falso Falso = Falso
inhibicionDer Falso Verdadero = Verdadero 
inhibicionDer Verdadero Falso = Falso 
inhibicionDer Verdadero Verdadero = Falso

-- 5. Transferencia (Derecha).
transfDer :: Booleano -> Booleano -> Booleano
transfDer Falso Falso = Falso
transfDer Falso Verdadero = Verdadero 
transfDer Verdadero Falso = Falso 
transfDer Verdadero Verdadero = Verdadero 

-- 6. Disyunción exclusiva (xor).
xor :: Booleano -> Booleano -> Booleano
xor Falso Falso = Falso
xor Falso Verdadero = Verdadero 
xor Verdadero Falso = Verdadero 
xor Verdadero Verdadero = Falso 

-- 7. Disyunción.
disyuncion :: Booleano -> Booleano -> Booleano
disyuncion Falso Falso = Falso
disyuncion Falso Verdadero = Verdadero 
disyuncion Verdadero Falso = Verdadero 
disyuncion Verdadero Verdadero = Verdadero 

-- 8. Nor (No or, negación de la disyunción).
nor :: Booleano -> Booleano -> Booleano
nor Falso Falso = Verdadero
nor Falso Verdadero = Falso 
nor Verdadero Falso = Falso 
nor Verdadero Verdadero = Falso 

-- 9. Equivalencia (Si y sólo si).
equiv :: Booleano -> Booleano -> Booleano
equiv Falso Falso = Verdadero
equiv Falso Verdadero = Falso 
equiv Verdadero Falso = Falso 
equiv Verdadero Verdadero = Verdadero 

-- 10. Complemento (Derecho).
complDer :: Booleano -> Booleano -> Booleano
complDer Falso Falso = Verdadero
complDer Falso Verdadero = Falso 
complDer Verdadero Falso = Verdadero 
complDer Verdadero Verdadero = Falso 

-- 11. Implicación (p -> q).
implDer :: Booleano -> Booleano -> Booleano
implDer Falso Falso = Verdadero
implDer Falso Verdadero = Falso 
implDer Verdadero Falso = Verdadero 
implDer Verdadero Verdadero = Verdadero 

-- 12. Complemento (Izquierdo).
complIzq :: Booleano -> Booleano -> Booleano
complIzq Falso Falso = Verdadero
complIzq Falso Verdadero = Verdadero 
complIzq Verdadero Falso = Falso 
complIzq Verdadero Verdadero = Falso 

-- 13. Implicación (q -> p).
implIzq :: Booleano -> Booleano -> Booleano
implIzq Falso Falso = Verdadero
implIzq Falso Verdadero = Verdadero 
implIzq Verdadero Falso = Falso 
implIzq Verdadero Verdadero = Verdadero 

-- 14. Nand (No and, negación de la conjuncion).
nand :: Booleano -> Booleano -> Booleano
nand Falso Falso = Verdadero
nand Falso Verdadero = Verdadero 
nand Verdadero Falso = Verdadero 
nand Verdadero Verdadero = Falso 

-- 15. Identidad (Constante binaria 1).
cte1 :: Booleano -> Booleano -> Booleano
cte1 _ _ = Verdadero

----------------------------------------------------------------------------------------------------
------                                                                                        ------
------                          Argumentos válidos                                            ------
------                                                                                        ------
----------------------------------------------------------------------------------------------------

-- Sinónimo para representar el conjunto de estados de una interpretación.
type Estados = [(String,Booleano)]

-- Función que regresa el resultado de evaluación de una fórmula proposicional dado un conjunto de
-- estados.
evalua :: Prop -> Estados -> Booleano
evalua (Cte c) lista = c
evalua (Var v) lista = busca v lista
evalua (Neg p) lista = negacion (evalua p lista)
evalua (Conj p q) lista = conjuncion (evalua p lista) (evalua q lista)
evalua (Disy p q) lista = disyuncion (evalua p lista) (evalua q lista)
evalua (Impl p q) lista = implIzq (evalua p lista) (evalua q lista)

busca :: String -> Estados -> Booleano
busca s lista = snd (head (filter (compara s) lista))

compara :: String -> (String,Booleano) -> Bool
compara s (v,f) = s == v

--Práctica 5
--Ejercicio 5.8

--Función recursiva que dada una fórmula proposicional, regresa una lista con las variables
--proposicionales de la fórmula.
vars :: Prop -> [String]
vars (Cte c) = []
vars (Var v) = [v]
vars (Neg p) = vars p
vars (Conj p q) = vars p ++ vars q
vars (Disy p q) = vars p ++ vars q
vars (Impl p q) = vars p ++ vars q 

-- Función recursiva que determina si una fórmula proposicional es tautología dada una lista de  
-- interpretaciones para las variables proposicionales. 
esTautologia :: Prop -> [Estados] -> Booleano
esTautologia p [] = Verdadero
esTautologia p (x:xs)  
    |"Falso" == show (evalua p x) =  Falso
    |otherwise = esTautologia p xs      

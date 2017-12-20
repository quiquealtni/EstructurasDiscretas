module Funciones where

-- Tipo de dato para representar funciones matemáticas
data Funcion = X 
             | Cte Float
             | Sum Funcion Funcion
             | Mul Funcion Funcion
             | Div Funcion Funcion
             | Pot Funcion Float 

-- Permite mostrar las funciones en un formato amigable
instance Show Funcion where
   show f = showFuncion f
   
-- Función que permite implementar la representación "amigable" de una función.
showFuncion :: Funcion -> String
showFuncion (X) = "x"
showFuncion (Cte a) = show a
showFuncion (Sum izq der) = showFuncion izq ++ " + " ++ showFuncion der
showFuncion (Mul izq der) = showFuncion izq ++ showFuncion der
showFuncion (Div izq der) = showFuncion izq ++ " / " ++ showFuncion der
showFuncion (Pot b n) = showFuncion b ++ "^" ++ show n 

evalua :: Funcion -> Float -> Float
evalua (X) x = x 
evalua (Cte a) c = a
evalua (Sum izq der) c = evalua (izq) c + evalua (der) c 
evalua (Mul izq der) c = evalua (izq) c * evalua (der) c
evalua (Div izq der) c = evalua (izq) c / evalua (der) c
evalua (Pot b n) c = evalua (b) c ** n

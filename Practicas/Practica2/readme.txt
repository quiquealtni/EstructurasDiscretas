Estructuras Discretas 2018-1
Practica 2

Autor:
- Altamirano Niño Luis Enrique


***Ejercicio 2.1
      1.- Traducir las siguientes expresiones matemáticas a expresiones de tipo Funcion
	a) x ---> X
	b) x + 2 ---> Sum X (Cte 2)
	c) 3x^2 + 2x + 3 ---> Sum (Mul (Cte 3) (Pot X 2)) (Sum (Mul (Cte 2) X) (Cte 3))
	d) x^3 + 7x^2 + 2x + 9 / 1835x^7 ---> Div (Sum (Sum (Pot X 3) (Mul (Cte 7) (Pot X 2))) (Sum (Mul (Cte 2) X) (Cte 9))) 
                                                  (Mul (Cte 1835) (Pot X 7))
***Ejercicio 2.2
      2.- Traducir las siguientes expresiones de la lógica proposicional a expresiones de tipo Prop
	a) ⊥ v T  --->  Disy (Cte Falso) (Cte Verdadero)
 	b) p v q  --->  Disy (Var "p") (Var "q")
        c) p → ¬r ∧ s  ---> Impl (Var "p") (Conj (Neg (Var "r")) (Var "s"))
	d) p ∨ q → r ↔ p ∧ q → ¬s  --->  Syss (Impl (Disy (Var "p") (Var "q"))(Var "r")) 
                                              (Impl (Conj (Var "p") (Var "q")) (Neg (Var "s")))



module Practica3 where

import Prop

----------------------------------------------------------------------------------------------------
------                                                                                        ------
------                          Fórmulas proposicionales                                      ------
------                                                                                        ------
----------------------------------------------------------------------------------------------------

-- Formula form
form = Impl (Conj (Impl (Var "p") (Var "q")) (Var "p")) (Var "q")


form1 = Impl (Conj (Impl (Var "p")(Var "q")) (Impl (Var "p") (Var "r"))) (Impl (Var "p") (Conj (Var "q") (Var "s")))
form2 = Impl (Conj (Conj (Var "p") (Var "q")) (Conj (Impl (Var "p") (Conj (Var "r") (Var "q"))) (Conj (Impl (Var "r") (Disy (Var "s") (Var "t"))) (Neg (Var "s")))) ) (Var "t")
form3 = Impl (Conj (Impl (Var "p") (Var "q")) (Conj (Disy (Var "p") (Var "r")) (Neg (Var "q")))) (Var "r")
form4 = Impl (Conj (Impl (Var "p") (Impl (Var "r") (Var "q"))) (Conj (Var "p") (Neg (Var "r")))) (Neg (Var "q")) 
form5 = Impl (Conj (Impl (Var "p") (Var "q")) (Conj (Impl (Var "r") (Var "s")) (Conj (Impl (Disy (Var "q") (Var "s")) (Var "t")) (Neg (Var "t"))))) (Neg (Conj (Var "p") (Var "r")))   

----------------------------------------------------------------------------------------------------
------                                                                                        ------
------                          Conjunto de interpretaciones                                  ------
------                                                                                        ------
----------------------------------------------------------------------------------------------------

-- Conjunto de posibles interpretaciones para las variables de form .
interps = [[("p",Falso),("q",Falso)],[("p",Falso),("q",Verdadero)],[("p",Verdadero),("q",Falso)],
    [("p",Verdadero),("q",Verdadero)]]

valores = [Falso,Verdadero]

interps1 = [[("p",a),("q",b),("r",c),("s",d)] | a <- valores, b <- valores , c <- valores , d <- valores]
interps2 = [[("p",a),("q",b),("r",c),("s",d),("t",e)] | a <- valores, b <- valores, c <- valores, d <- valores, e <- valores]
interps3 = [[("p",a),("q",b),("r",c)] | a <- valores, b <- valores , c <- valores]
interps4 = interps3
interps5 = interps2

----------------------------------------------------------------------------------------------------
------                                                                                        ------
------                          Conjunto de interpretaciones                                  ------
------                                                                                        ------
----------------------------------------------------------------------------------------------------

-- Verificación de form
res = esTautologia form interps

res1 = esTautologia form1 interps1
res2 = esTautologia form2 interps2
res3 = esTautologia form3 interps3
res4 = esTautologia form4 interps4
res5 = esTautologia form5 interps5

----------------------------------------------------------------------------------------------------
------                                                                                        ------
------                          Ejecución del programa                                        ------
------                              (No modificar)                                            ------
------                                                                                        ------
----------------------------------------------------------------------------------------------------
runPractica3 = 
   do
      putStr ("Argumento ejemplo: " ++ show form ++ "\n")
      putStr ("Válido: " ++ (show res) ++ "\n")

      putStr "\n"

      putStr ("Argumento 1: " ++ show form1 ++ "\n")
      putStr ("Válido: " ++ (show res1) ++ "\n")

      putStr "\n"

      putStr ("Argumento 2: " ++ show form2 ++ "\n")
      putStr ("Válido: " ++ (show res2) ++ "\n")

      putStr "\n"

      putStr ("Argumento 3: " ++ show form3 ++ "\n")
      putStr ("Válido: " ++ (show res3) ++ "\n")
      
      putStr "\n"

      putStr ("Argumento 4: " ++ show form4 ++ "\n")
      putStr ("Válido: " ++ (show res4) ++ "\n")
      
      putStr "\n"

      putStr ("Argumento 5: " ++ show form5 ++ "\n")
      putStr ("Válido: " ++ (show res5) ++ "\n")

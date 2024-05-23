module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {Mapaluva}
-- Integrante1: { 47168772,De Girolamo Valentin Gabriel}
-- Integrante2: { 46501918,Risso Mateo Angel}
-- Integrante3: { 45478261,Masnatta Bagnes Paula Sofia}
-- Integrante4: { 45076050,Rojas Lucas Mariano}
-- Integrantes que abandonaron la materia: {}

----------------------------------------------------------------------------------------------------------------                       

posicionVocales :: Char -> [Char] -> Int
posicionVocales _ [] = 1
posicionVocales x (y:ys) | x == y = 0
                         | otherwise = 1 + (posicionVocales x ys)

--En esta funcion, "x" es "letra" e "(y:ys)" es abecedario
--Explicación-- Esto devuelve la posición de el elemento (representado como letra) respecto a la lista (llamado abecedario).
--Si el elemento no esta en la lista, el resultado es dos unidades mayor a el tamaño de la lista



-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula x = posicionVocales x ("abcdefghijklmnopqrstuvwxyz") < 26

--En esta función, "x" es la letra elegida por el usuario
--Explicación-- Se usa la función anterior ("posicionVocales"), teniendo como inputs la letra elegida por el usuario y el abecedario.
--Es verdadero si el resultado es menor o igual a la longitud de la lista



----------------------------------------------------------------------------------------------------------------

-- EJ 2
letraANatural :: Char -> Int
letraANatural x = posicionVocales x ("abcdefghijklmnopqrstuvwxyz")

--En esta función, "x" es la letra elegida por el usuario
--Explicacion-- Usa la funcion ya nombrada ("posicionVocales"), la cual devuelve en que posición esta el caracter respecto a la lista
--Simplemente se definen los inputs como: La letra del usuario, y el abecedario




----------------------------------------------------------------------------------------------------------------

vocalSegunPosicion :: Int -> [Char] -> Char
vocalSegunPosicion 0 (y:ys) = y
vocalSegunPosicion x (y:ys) | x > 25 = vocalSegunPosicion (x-26) (y:ys)
                            | x < (-25) = vocalSegunPosicion (x+26) (y:ys)
                            | x > (-25) && x < 0 = vocalSegunPosicion (26+x) (y:ys)
                            | otherwise = vocalSegunPosicion (x-1) ys

--En esta función, "x" es el numero natural (incluyendo a 0) e "(y:ys)" es el abecedario
--Explicación-- Toma un numero natural (incluyendo a 0), de ser negativo lo pasa a natural, y llega al elemento en su posición en la lista abecedario. Si el numero supera a 25, usa las congruencias                            


-- EJ 3
desplazar :: Char -> Int -> Char
desplazar y x | (esMinuscula y) == False = y
              | otherwise = vocalSegunPosicion ((letraANatural y) + x) ("abcdefghijklmnopqrstuvwxyz")

--En esta función, "x" es el numero dado por el usuario e "y" es la letra
--Explicación-- Primero busca la posición de la letra, luego le suma el numero dado por el usuario, finalmente devuelve la letra de la respectiva posición



------------------------------------------------------------------------------------------------------------------

-- EJ 4
cifrar :: String -> Int -> String
cifrar "" _ = ""
cifrar (y:ys) x = desplazar y x : cifrar ys x

--Explicación-- Aplica los algoritmos anteriores (aplica ""desplazar", por lo tanto los algoritmos usados en esa función) a una oración



------------------------------------------------------------------------------------------------------------------
-- EJ 5
descifrar :: String -> Int -> String
descifrar "" _ = ""
descifrar (y:ys) x = (desplazar y (26 - x) : descifrar ys x)

--En esta función "x" es el numero, "(y:ys)" es el string (donde y es la primer letra del string e ys es el resto de la palabra sin la primer letra)
--Explicacion-- Aplica un algoritmo igual al anterior pero cambiando el numero de tal manera que descifre



------------------------------------------------------------------------------------------------------------------

cifrarListaAuxiliar :: [String] -> Int -> [String]
cifrarListaAuxiliar [] _ = []
cifrarListaAuxiliar (x:xs) n = (cifrar x n : cifrarListaAuxiliar xs (n + 1))

--En esta función "(x:xs)" es la lista, y "n" el n actual
--Explicación-- Aplica cifrar a cada elemento de la lista. Se usa mediante función auxiliar para usar la iteración

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista (x:xs) = cifrarListaAuxiliar (x:xs) 0

--En esta función "(x:xs)" es la lista
--Explicación-- Llama a la funcion auxiliar



-----------------------------------------------------------------------------------------------------------------

contarApariciones :: Char -> String -> Int
contarApariciones _ "" = 0
contarApariciones l (o:os) | l == o = 1 + contarApariciones l os
                           | l /= o = contarApariciones l os

--En esta función "l" es la letra y "(o:os)" es la oración
--Explicación-- Cuenta cuantas veces aparece letra en oración



listaConApariciones :: String -> String -> [Int]
listaConApariciones _ "" = []
listaConApariciones o (a:abc) = contarApariciones a o : listaConApariciones o abc

--En esta función "o" es la oracion y "(a:abc)" es el abecedario
--Explicación-- Cuenta cuantas veces aparece cada letra del abecedario en una oración, usando la función anterior



aPorcentaje :: Int -> [Int] -> [Float]
aPorcentaje _ [] = []
aPorcentaje x (y:ys) = ((((fromIntegral y) / (fromIntegral x))*100) : aPorcentaje x ys)

--En esta función "x" es el "largo" (longitud de la oración) e "(y:ys)" es la lista 
--Explicación-- Toma la longitud de la oración (como "largo") y la lista con cuantas apariciones tiene la letra en la oración
--Con lo anterior, se toma cada cantidad de apariciones, y mediante la división con la longitud de la lista, se tiene el porcentaje



-- EJ 7
frecuencia :: String -> [Float]
frecuencia "" = frecuencia "<" 
frecuencia o = aPorcentaje (length o) (listaConApariciones o "abcdefghijklmnopqrstuvwxyz")

--En el caso base : frecuencia "" = frecuencia "<" 
--Cuando la lista es vacía, en vez de aparecer 0 aparece NaN, por lo tanto se cambia a un digito no minuscula



-------------------------------------------------------------------------------------------------------------------

buscarMayor :: [Float] -> Float
buscarMayor (x1:x2:xs) | x1 >= x2 && (xs == []) = x1
                       |x1 < x2 && (xs == []) = x2
                       |x1 >= x2 = buscarMayor (x1:xs)
                       |x1 < x2 = buscarMayor (x2:xs)

--Explicación-- Va iterando el mayor de los primeros dos elementos hasta que se acabe la lista



posicionFloat :: Float -> [Float] -> Int
posicionFloat x (y:ys) | x == y = 0
                       |otherwise = 1 + (posicionFloat x ys)

--En esta función "x" es el numero e "(y:ys)" es la lista 
--Explicación-- Devuelve la posición de un numero en una lista



frecEJ8 :: String -> Int -> [Float]
frecEJ8 or pos = frecuencia (cifrar or pos)

--En esta función "or" es la oración y "pos" es la posición
--Explicacion-- Es una manera de resumir el codigo del ejercicio 8



-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente o pos = (vocalSegunPosicion (posicionFloat (buscarMayor (frecEJ8 o pos)) (frecEJ8 o pos) ) "abcdefghijklmnopqrstuvwxyz" , buscarMayor (frecEJ8 o pos))
--Explicacion-- Es una recopilación de funciones anteriores. Siendo o=oración y pos=posición



-------------------------------------------------------------------------------------------------------------------

esDescifradoAuxiliar :: String -> String -> Int -> Bool
esDescifradoAuxiliar or orc (-1) = False
esDescifradoAuxiliar or orc pos | (cifrar or pos) == orc = True
                                | otherwise = esDescifradoAuxiliar or orc (pos - 1)

--En esta funcion "or" es la oración, "orc" es la oración cifrada y "pos" es la posición
--Explicación-- Por cada posición posible, verifica si ambos string son de alguna manera el cifrado del otro

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado or orc = esDescifradoAuxiliar or orc 25

--En esta funcion "or" es la oración y "orc" es la oración cifrada
--Explicación-- Le agrega el parametro 25 a la anterior función. 25 representa la cantidad de vocales posibles



--------------------------------------------------------------------------------------------------------------------

recursionLista :: String -> [String] -> [String]
recursionLista p [] = []
recursionLista p (l:ls) | esDescifrado p l = l : recursionLista p ls
                        | otherwise = recursionLista p ls

--En esta función "p" es la palabra y "(l:ls)" es la lista
--Explicación-- Recorre cada elemento de la lista. Y verifica si cada elemento es el elemento palabra pero cifrado



fusionarTuplas :: String -> [String] -> [(String,String)]
fusionarTuplas p [] = []
fusionarTuplas p (l:ls) = (p,l) : (l, p) : fusionarTuplas p ls

--En esta función "p" es la palabra y "(l:ls)" es la lista
--Explicación-- Toma una palabra y una lista de elementos, y por cada elemento hace una lista tal que (palabra, elemento)
--Tambien devuelve (elemento,palabra) porque cuando elemento sea palabra, seria asi



-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [x] | x == (cifrar x 2) = [(x,x)]
todosLosDescifrados (x:xs) |x == (cifrar x 2) = [(x,x)] ++ todosLosDescifrados xs
                           |xs == [] = []
                           |otherwise = fusionarTuplas x (recursionLista x xs) ++ todosLosDescifrados xs

--Explicación-- Mediante las dos funciones anteriores, para cada elemento se agrega todas sus similitudes
-- Si x es igual a x cifrado, podemos concluir que no hay minusculas, asique lo relacionamos consigmo mismo(porque cumple la consigna), y lo sacamos(porque no se puede cifrar)



---------------------------------------------------------------------------------------------------------------------

descontarLetras :: String -> Int -> String
descontarLetras _ 0 = ""
descontarLetras (p:ps) aContar = (p : descontarLetras ps (aContar - 1))

--En esta función "(p:ps)" es la palabra 
--Explicacion-- Se toma una palabra y se queda con las primeras "Acontar" letras



-- EJ 11
expandirClave :: String -> Int -> String
expandirClave p l | ((length p)) <= l = p ++ expandirClave p (l - (length p))
                  | otherwise = descontarLetras p l
                  
--En esta función "p" es la palabra y "l" la longitud 
--Explicación-- Se repite la palabra para que tenga las letras especificadas, dividiendola si no son proporcionales

----------------------------------------------------------------------------------------------------------------------

cifrarVigenereAuxiliar :: String -> String -> Int-> String
cifrarVigenereAuxiliar "" _ _ = ""
cifrarVigenereAuxiliar (p:ps) (c:cs) s = desplazar p (s*(letraANatural c)) : cifrarVigenereAuxiliar ps cs s 

--En esta función, "(p:ps)" es la palabra, "(c:cs)" es el codigo y "s" es el signo
--Explicacion-- Por cada letra, la despslaza por el desplazamiento de la letra correspondiente del codigo



-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere palabra codigo = cifrarVigenereAuxiliar palabra (expandirClave codigo (length palabra)) (1)

--Explicación-- Llama a la función auxiliar con el código expandido


----------------------------------------------------------------------------------------------------------------------

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere palabra codigo = cifrarVigenereAuxiliar palabra (expandirClave codigo (length palabra)) (-1)

--Explicación-- Hace lo mismo que "cifrarVigenere" pero convierte en negativo el desplazamiento



----------------------------------------------------------------------------------------------------------------------

contarDistancia :: String -> String -> Int
contarDistancia "" _ = 0
contarDistancia (p:ps) (c:cs) = (abs ((letraANatural p) - (letraANatural c))) + contarDistancia ps cs

--En esta función, "(p:ps)" es la palabra y "(c:cs)" es el codigo
--Explicación-- Cuenta la distancia entre dos Strings (con la definición de distancia de la especificación)



buscarPeor :: String -> [String] -> (String, Int) -> String
buscarPeor _ [] (e,c) = e
buscarPeor p (x:xs) (e,c)|c>(contarDistancia p (cifrarVigenere p x)) = buscarPeor p xs (x, contarDistancia p (cifrarVigenere p x))
    |otherwise = buscarPeor p xs (e, c)
--Explicacion-- e es el cifrado, c es la distancia entra s y s ya cifrado(esto para gastar menos recursos y no calcularlo todo el tiempo)
--Se recursiona la lista hasta quedarse con el termino mayor



-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado p (x:xs) = buscarPeor p xs (x, contarDistancia p (cifrarVigenere p x))

--En esta función "p" es la palabra y "(x:xs)" son las claves
--Explicacion-- Llama a la función "buscarPeor" con el primer elemento como elegido



-----------------------------------------------------------------------------------------------------------------------

todosVigenere :: String -> [String] -> String -> [(String, String)]
todosVigenere _ [] _ = []
todosVigenere p (l:ls) c | (cifrarVigenere p l) == c = ((p, l) : todosVigenere p ls c)
                         | otherwise = todosVigenere p ls c

--En esta función "p" es la palabra, "(l:ls)" es la lista y "c" es el código
--Explicación--Para el elemento palabra, revisa si mediante cada elemento de lista, se puede llegar a código

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] _ _ = []
combinacionesVigenere (msj:msjs) claves cifr = todosVigenere msj claves cifr ++ combinacionesVigenere msjs claves cifr

--En esta función "(msj:msjs)" son msjs y "cifr" es el cifrado 
--Explicación--Para cada elemento de la lista usa todosVigenere
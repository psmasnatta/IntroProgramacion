import Test.HUnit
import Solucion
import Data.List
-- No está permitido agregar nuevos imports.

runCatedraTests = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testsEsMinuscula,
    "letraANatural" ~: testsLetraANatural,
    "desplazar" ~: testsDesplazar,
    "cifrar" ~: testsCifrar,
    "descifrar" ~: testsDescifrar,
    "cifrarLista" ~: testsCifrarLista,
    "frecuencia" ~: testsFrecuencia,
    "cifradoMasFrecuente" ~: testsCifradoMasFrecuente,
    "esDescifrado" ~: testsEsDescifrado,
    "todosLosDescifrados" ~: testsTodosLosDescifrados,
    "expandirClave" ~: testsExpandirClave,
    "cifrarVigenere" ~: testsCifrarVigenere,
    "descifrarVigenere" ~: testsDescifrarVigenere,
    "peorCifrado" ~: testsPeorCifrado,
    "combinacionesVigenere" ~: testsCombinacionesVigenere
    ]


testsEsMinuscula = test [
    "esMinuscula Caso 1" ~: esMinuscula 'd' ~?= True,
    "esMinuscula Caso 2" ~: esMinuscula 'a' ~?= True,
    "EsMinuscula Caso 3" ~: esMinuscula 'z' ~?= True,
    "EsMinuscula Caso 4" ~: esMinuscula 'D' ~?= False,
    "EsMinuscula Caso 5" ~: esMinuscula 'A' ~?= False,
    "EsMinuscula Caso 6" ~: esMinuscula ' ' ~?= False    
    ]

testsLetraANatural = test [
    "letraANatural Caso 1" ~: letraANatural 'b' ~?= 1,
    "letraANatural Caso 2" ~: letraANatural 'a' ~?= 0,
    "letraANatural Caso 3" ~: letraANatural 'k' ~?= 10,
    "letraANatural Caso 4" ~: letraANatural 'p' ~?= 15,
    "letraANatural Caso 5" ~: letraANatural 'w' ~?= 22,
    "letraANatural Caso 6" ~: letraANatural 'z' ~?= 25
    ]

testsDesplazar = test [
    "Desplazar Caso 1" ~: desplazar 'a' 3 ~?= 'd',
    "Desplazar Caso 2" ~: desplazar 'A' 3 ~?= 'A',
    "Desplazar Caso 3" ~: desplazar 'a' 26 ~?= 'a',
    "Desplazar Caso 4" ~: desplazar 'a' 28 ~?= 'c',
    "Desplazar Caso 5" ~: desplazar 'z' 1 ~?= 'a',
    "Desplazar Caso 6" ~: desplazar 'a' 0 ~?= 'a'
    ]

testsCifrar = test [
    "Cifrar Caso 1" ~: cifrar "computacion" 3 ~?= "frpsxwdflrq",
    "Cifrar Caso 2" ~: cifrar "COMPUTACION" 5 ~?= "COMPUTACION",
    "Cifrar Caso 3" ~: cifrar "" 3 ~?= "",
    "Cifrar Caso 4" ~: cifrar "zorro" 5 ~?= "etwwt",
    "Cifrar Caso 5" ~: cifrar "amigos" 28 ~?= "cokiqu"
    ]

testsDescifrar = test [
    "Descifrar Caso 1" ~: descifrar "frpsxwdflrq" 3 ~?= "computacion",
    "Descifrar Caso 2" ~: descifrar "COMPUTACION" 5 ~?= "COMPUTACION",
    "Descifrar Caso 3" ~: descifrar "" 3 ~?= "",
    "Descifrar Caso 4" ~: descifrar "etwwt" 5 ~?= "zorro",
    "Descifrar Caso 5" ~: descifrar "cokiqu" 28 ~?= "amigos"
    ]

testsCifrarLista = test [
    "CifrarLista Caso 1" ~: cifrarLista ["compu","labo","intro"] ~?= ["compu","mbcp","kpvtq"],
    "CifrarLista Caso 2" ~: cifrarLista ["amigos","amigos","amigos","AMIGOS"] ~?= ["amigos","bnjhpt","cokiqu","AMIGOS"],
    "CifrarLista Caso 3" ~: cifrarLista ["compu","zorro","COMpa"] ~?= ["compu","apssp","COMrc"]
    ]

testsFrecuencia = test [
    "Frecuencia Caso 1" ~: expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0],
    "Frecuencia Caso 2" ~: expectlistProximity (frecuencia "amiguitos") [11.111111,0.0,0.0,0.0,0.0,0.0,11.111111,0.0,22.222222,0.0,0.0,0.0,11.111111,0.0,11.111111,0.0,0.0,0.0,11.111111,11.111111,11.111111,0.0,0.0,0.0,0.0,0.0], 
    "Frecuencia Caso 3" ~: frecuencia "pythOn" ~?= [0.0,0.0,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,16.666668,0.0],
    "Frecuencia Caso 4" ~: frecuencia "TALLER" ~?= [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

testsCifradoMasFrecuente = test [
    "CifradoMasFrecuente Caso 1" ~: cifradoMasFrecuente "taller" 3 ~?= ('o',33.333336),
    "CifradoMasFrecuente Caso 2" ~: expectAnyTuplaAprox (cifradoMasFrecuente "pappo" 5) [('u',60.0)], 
    "CifradoMasFrecuente Caso 3" ~: cifradoMasFrecuente "PIOLa" 0 ~?= ('a',20.0),
    "CifradoMasFrecuente Caso 4" ~: cifradoMasFrecuente "piola" 1 ~?= ('b',20.0)
    ]

testsEsDescifrado = test [
    "EsDescifrado Caso 1" ~: esDescifrado "taller" "compu" ~?= False,
    "EsDescifrado Caso 2" ~: esDescifrado "computacion" "frpsxwdflrq" ~?= True,
    "EsDescifrado Caso 3" ~: esDescifrado "computacion" "frpsxwdflrl" ~?= False,
    "EsDescifrado Caso 4" ~: esDescifrado "COMPUTACION" "frpsxwdflrq" ~?= False,
    "EsDescifrado Caso 5" ~: esDescifrado "COMPUTACION" "COMPUTACION" ~?= True 
    ]

testsTodosLosDescifrados = test [
    "TodosLosDescifrados Caso 1" ~: todosLosDescifrados ["compu", "frpsx", "mywza"] ~?= [("compu", "frpsx"), ("frpsx", "compu")], 
    "TodosLosDescifrados Caso 2" ~: expectPermutacion (todosLosDescifrados ["computacion", "dpnqvubdjpo", "frpsxwdflrq"]) [("computacion", "dpnqvubdjpo"), ("dpnqvubdjpo", "computacion"), ("computacion", "frpsxwdflrq"), ("frpsxwdflrq", "computacion"), ("frpsxwdflrq", "dpnqvubdjpo"), ("dpnqvubdjpo", "frpsxwdflrq")],    
    "TodosLosDescifrados Caso 3" ~: todosLosDescifrados ["a","b","cd","A"] ~?= [("a","b"),("b","a"),("A","A")],
    "TodosLosDescifrados Caso 4" ~: todosLosDescifrados ["ab","cd","efg",""] ~?= [("ab","cd"),("cd","ab"),("","")]
    ]

testsExpandirClave = test [
    "ExpandirClave Caso 1" ~: expandirClave "compu" 8 ~?= "compucom",
    "ExpandirClave Caso 2" ~: expandirClave "alfabeto" 2 ~?= "al",
    "ExpandirClave Caso 3" ~: expandirClave "zapatilla" 9 ~?= "zapatilla",
    "ExpandirClave Caso 4" ~: expandirClave "palabras" 20 ~?= "palabraspalabraspala"
    ]

testsCifrarVigenere = test [
    "CifrarVigenere Caso 1" ~: cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv",
    "CifrarVigenere Caso 2" ~: cifrarVigenere "amiguitos" "ip" ~?= "ibqvcxbda",
    "CifrarVigenere Caso 3" ~: cifrarVigenere "ingles" "python" ~?= "xlzssf",
    "CifrarVigenere Caso 4" ~: cifrarVigenere "algo" "algoritmo" ~?= "awmc"
    ]

testsDescifrarVigenere = test [
    "DescifrarVigenere Caso 1" ~: descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
    "DescifrarVigenere Caso 2" ~: descifrarVigenere "ibqvcxbda" "ip" ~?= "amiguitos",
    "DescifrarVigenere Caso 3" ~: descifrarVigenere "xlzssf" "python" ~?= "ingles",
    "DescifrarVigenere Caso 4" ~: descifrarVigenere "awmc" "algoritmo" ~?= "algo"
    ]

testsPeorCifrado = test [
    "PeorCifrado Caso 1" ~: peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef",
    "PeorCifrado Caso 2" ~: peorCifrado "amigos" ["zzz","amigos","palabra"] ~?= "zzz",
    "PeorCifrado Caso 3" ~: peorCifrado "computacion" ["aaa","bbb","ccc"] ~?= "aaa",
    "PeorCifrado Caso 4" ~: peorCifrado "zorro" ["mnmn","abab","lmlm","bbbb"] ~?= "abab"
    ]

testsCombinacionesVigenere = test [
    "CombinacionesVigenere Caso 1" ~: combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")],
    "CombinacionesVigenere Caso 2" ~: combinacionesVigenere ["amigos","bnjhpt"] ["a", "z"] "amigos" ~?= [("amigos", "a"),("bnjhpt", "z")],
    "CombinacionesVigenere Caso 3" ~: combinacionesVigenere ["piola","computacion"] ["n", "ip"] "kdueciirqdv" ~?= [("computacion", "ip")],
    "CombinacionesVigenere Caso 4" ~: combinacionesVigenere ["algoritmo","dato"] ["p","l"] "ebup" ~?= []
    ]

-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)
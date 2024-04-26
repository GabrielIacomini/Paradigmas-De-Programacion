-- EJERCICIO 1 --
-- Definir una función que sume una lista de números --

sumarNumerosDeLista :: [Int] -> Int
sumarNumerosDeLista  = sum 

-- EJERCICIO 2 --

frecuenciaCardiaca :: [Double]
frecuenciaCardiaca =  [80, 100, 120, 128, 130, 123, 125]

-- a --
-- Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la frecuencia cardíaca --

promedioFrecuenciaCardiaca :: Double
promedioFrecuenciaCardiaca  = sum frecuenciaCardiaca / fromIntegral (length frecuenciaCardiaca)

-- b --
-- Definir la función frecuenciaCardiacaMinuto/1, que recibe m que es el minuto en el cual quiero conocer la frecuencia cardíaca, m puede ser a los 10, 20, 30 ,40,..hasta 60. --

frecuenciaCardiacaMinuto :: Int -> Double
frecuenciaCardiacaMinuto instante 
    | instante `elem` [0, 10, 20, 30, 40, 50, 60] = frecuenciaCardiaca !! ( instante `div` 10 )
    | otherwise = error "Minuto no valido"

-- c --
-- Definir la función frecuenciasHastaMomento/1, devuelve el total de frecuencias que se obtuvieron hasta el minuto m. --

frecuenciasHastaMomento :: Int -> [Double]
frecuenciasHastaMomento instante
    | instante > 60 = error "Minuto no valido"
    | otherwise = take (instante `div` 10 + 1)  frecuenciaCardiaca

-- EJERCICIO 3 --
-- Definir la función esCapicua/1, si data una lista de listas, me devuelve si la concatenación de las sublistas es una lista capicua --

esCapicua :: Eq a => [[a]] -> Bool
esCapicua listaDeListas = concat listaDeListas == reverse (concat listaDeListas)

-- EJERCICIOS --
-- 2 --
-- Definir la función mejor/3, que recibe dos funciones y un número, y devuelve el resultado de la función que dé un valor más alto --

-- Funciones auxiliares doble y cuadrado --
cuadrado :: Int -> Int
cuadrado num = num * num

triple :: Int -> Int
triple num = num * 3

mejor :: (Num a, Ord b) => (a -> b) -> (a -> b) -> a -> b
mejor funcion1 funcion2 numero = max (funcion1  numero) (funcion2 numero)

-- 3 --
-- Definir la función aplicarPar/2, que recibe una función y un par, y devuelve el par que resulta de aplicar la función a los elementos del par --

aplicarPar :: (a -> b) -> (a, a) -> (b, b)
aplicarPar func (valor1, valor2) = (func valor1, func valor2)

-- 4 --
-- Definir la función parDeFns/3, que recibe dos funciones y un valor, y devuelve un par ordenado que es el resultado de aplicar las dos funciones al valor --

parDeFns :: (a -> b) -> (a -> a) -> a -> (b, a)
parDeFns func1 func2 valor = (func1 (func2 valor), func2 valor)

-- ORDEN SUPERIOR + LISTAS --
-- 1 --
-- Definir la función esMultiploDeAlguno/2, que recibe un número y una lista y devuelve True si el número es múltiplo de alguno de los números de la lista --

esMultiploDe :: Int -> Int -> Bool
esMultiploDe numero1  = (== 0) . mod numero1

esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno num  = any (esMultiploDe num) 

-- 2 --
-- Armar una función promedios/1, que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento --

promedios :: [[Float]] -> [Float]
promedios listaDeListas = map promedioLista listaDeListas
    where promedioLista listaDeListas = sum listaDeListas / fromIntegral ( length listaDeListas )

-- 3 --
-- Armar una función promediosSinAplazos que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento, excluyendo los que sean menores a 4 que no se cuentan --

--promediosSinAplazos :: [[]]--

-- 4 --
-- Definir la función mejoresNotas, que dada la información de un curso devuelve la lista con la mejor nota de cada alumno --

mejoresNotas :: [[Int]] -> [Int]
mejoresNotas  = map maximum

-- 5 --
-- Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True si el alumno aprobó. Se dice que un alumno aprobó si todas sus notas son 6 o más --

aprobo :: [Int] -> Bool
aprobo = all (>= 6)

aprobo' :: [Int] -> Bool
aprobo' lista
    | minimum lista >=6 = True
    | otherwise = False

-- 6 --
-- Definir la función aprobaron/1, que dada la información de un curso devuelve la información de los alumnos que aprobaron --

aprobaron :: [[Int]] -> [[Int]]
aprobaron listasCurso = filter aprobo listasCurso 

-- 7 --
-- Definir la función divisores/1, que recibe un número y devuelve la lista de divisores --

divisores :: Int -> [Int]
divisores n = filter (esMultiploDe n) [1..n]

-- 8 --
-- Definir la función exists/2, que dadas una función booleana y una lista devuelve True si la función da True para algún elemento de la lista --

exists :: (Int -> Bool) -> [Int] -> Bool
exists = any 
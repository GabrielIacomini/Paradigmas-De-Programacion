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


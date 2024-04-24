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

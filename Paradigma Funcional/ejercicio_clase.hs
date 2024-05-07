data Elemento = Elemento {
    nombreElemento :: String,
    simboloQuimico :: String,
    numeroAtomico :: Int,
    grupoElemento :: Grupo
} deriving (Show, Eq)

data Sustancia = SustanciaSimple Elemento | SustanciaCompuesta {nombreCompuesto :: String, componentes :: [(Sustancia, Int)], grupoCompuesto :: Grupo} deriving (Show, Eq)

data Grupo = Metal | NoMetal | Halogeno | GasNoble deriving (Show, Eq)

-- 1 --
-- a --
hidrogeno :: Sustancia
hidrogeno = SustanciaSimple (Elemento "Hidrogeno" "H" 1 NoMetal)

oxigeno :: Sustancia
oxigeno = SustanciaSimple (Elemento "Oxigeno" "O" 8 NoMetal)

-- b --
agua :: Sustancia
agua =  SustanciaCompuesta "Agua" [(hidrogeno, 2), (oxigeno, 1)] NoMetal

-- 2 --

data Criterio = Electricidad | Calor 

conduceBien :: Sustancia -> Criterio -> String
conduceBien sustancia criterio
    | esMetal sustancia = "conductor"
    | esGasNoble sustancia criterio =  "conductor"
    | esHalogeno sustancia criterio =  "conductor"
    | otherwise = "No es conductor"
    where 
        esMetal (SustanciaSimple elemento) = grupoElemento elemento == "Metal"
        esMetal _ = False

        esGasNoble (SustanciaSimple elemento) Electricidad = grupoElemento elemento == "Gas Noble"
        esGasNoble _ _ = False

        esHalogeno (SustanciaSimple elemento) Calor =  grupoElemento elemento == "Halogeno"
        esHalogeno _ _ = False




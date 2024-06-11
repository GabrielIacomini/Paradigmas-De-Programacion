--1
data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgaste :: Desgaste, --Se podria hacer como (Float, Float)
    velMaxima :: Float,
    tiempoDeCarrera :: Float
} deriving (Show)

type Desgaste = (Float, Float)

chasis :: Desgaste -> Float
chasis = fst

ruedas :: Desgaste -> Float
ruedas = snd

--a
ferrari :: Auto
ferrari = Auto "Ferrari" "F50" (0,0) 65 0

--b
lambo :: Auto
lambo = Auto "Lamborghini" "Diablo" (7,4) 73 0

--c
fiat :: Auto
fiat = Auto "Fiat" "600" (33,27) 44 0

--2
--a
autoEnBuenEstado :: Auto -> Bool
autoEnBuenEstado = estaBien . desgaste

estaBien :: Desgaste -> Bool
estaBien unDesgaste = chasis unDesgaste < 40 && ruedas unDesgaste < 60

--b
autoNoDaMas :: Auto -> Bool
autoNoDaMas  = estaMal . desgaste

estaMal:: Desgaste -> Bool
estaMal unDesgaste = chasis unDesgaste > 80 || ruedas unDesgaste > 80

--3
repararUnAuto :: Auto -> Auto
repararUnAuto = aplicarDesgasteChasis (* 0.15) . aplicarDesgasteRuedas (const 0)


--4
type Tramo = Auto -> Auto
--a
curva :: Float -> Float -> Tramo
curva unAngulo unaLongitud unAuto =
    aplicarDesgasteRuedas (+ (3 * unaLongitud / unAngulo)) . aumentarTiempo (unaLongitud / (velMaxima unAuto / 2)) $ unAuto

--Delegamos las funciones de desgaste del chasis, las ruedas y aumentar el tiempo
aplicarDesgasteChasis :: (Float -> Float) -> Auto -> Auto
aplicarDesgasteChasis func unAuto = unAuto {
    desgaste = ((func . chasis . desgaste ) unAuto , ruedas (desgaste unAuto))
}

aplicarDesgasteRuedas :: (Float -> Float) -> Auto -> Auto
aplicarDesgasteRuedas func unAuto = unAuto {
    desgaste = ( chasis (desgaste unAuto), (func . ruedas . desgaste ) unAuto)
}

aumentarTiempo :: Float -> Auto -> Auto
aumentarTiempo unTiempo unAuto = unAuto {
    tiempoDeCarrera = tiempoDeCarrera unAuto + unTiempo
}


--a.i 
curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300

--a.ii
curvaTranca :: Tramo
curvaTranca = curva 110 550

--b
recta :: Float -> Tramo
recta unaLongitud unAuto =  aplicarDesgasteChasis (+ (unaLongitud / 100)) . aumentarTiempo (unaLongitud / velMaxima unAuto) $ unAuto

--b.i 
tramoRectoClassic :: Tramo
tramoRectoClassic = recta 750

--b.ii 
tramito :: Tramo
tramito = recta 280

--c
boxes :: Tramo -> Tramo
boxes unTramo unAuto
    | autoEnBuenEstado unAuto = unTramo unAuto
    | otherwise       = aumentarTiempo 10 . repararUnAuto $ unAuto

--d
mojarUnTramo :: Tramo -> Tramo
mojarUnTramo unTramo unAuto =
    aumentarTiempo segundosPorMojado . unTramo $ unAuto
    where
        segundosPorMojado = (tiempoDeCarrera (unTramo unAuto) - tiempoDeCarrera unAuto) / 2

--e
ripio :: Tramo -> Tramo
ripio unTramo = unTramo . unTramo

--f
obstruccion :: Tramo -> Float -> Tramo
obstruccion unTramo unaLongitud unAuto =
    aplicarDesgasteRuedas (+ (unaLongitud * 2)) . unTramo $ unAuto

--5
pasarPorTramo :: Auto -> Tramo -> Auto
pasarPorTramo unAuto unTramo
    | autoNoDaMas unAuto = unAuto
    | otherwise          = unTramo unAuto

--6
type Pista = [Tramo]

--a
superPista :: Pista
superPista = [
        tramoRectoClassic,
        curvaTranca,
        tramito . mojarUnTramo tramito,
        obstruccion (curva 80 400) 2,
        curva 115 650,
        recta 970,
        curvaPeligrosa,
        ripio tramito,
        boxes (recta 800)
    ]

--b
peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta unaPista = filter (not. autoNoDaMas) . map (pegarUnaVuelta unaPista)

pegarUnaVuelta :: Pista -> Auto -> Auto
pegarUnaVuelta unaPista unAuto = foldl pasarPorTramo unAuto unaPista
-- SI LO HAGO CON FOLDR NO HACE FALTA CAMBIAR EL ORDEN DE LAS VARIABLES EN pasarPorTramo

--7
--a
data Carrera = Carrera {
    pista :: Pista,
    cantidadVueltas :: Int
}

--b
tourBuenosAires :: Carrera
tourBuenosAires = Carrera superPista 20

--c
correrUnaCarrera :: [Auto] -> Carrera -> [[Auto]]
correrUnaCarrera unosAutos unaCarrera = 
    take (cantidadVueltas unaCarrera) . iterate (peganLaVuelta (pista unaCarrera)) $ unosAutos

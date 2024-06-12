import Text.Show.Functions()

--1
data Plomero = Plomero {
    nombre :: String,
    cajaDeHerramientas :: [Herramienta],
    historialReparaciones :: [Reparacion],
    dinero :: Float
} deriving (Show)

data Herramienta = Herramienta {
    denominacion :: String,
    precio :: Float,
    materialEmpuniadura :: Material
} deriving (Show, Eq)

data Material = Hierro | Madera | Goma | Plastico deriving (Show, Eq)

--a
mario :: Plomero
mario = Plomero "Mario" [llaveInglesa, martillo] [] 1200

--b
wario :: Plomero
wario = Plomero "Wario" (iterate (aumentarPrecio 1) llaveFrancesa) [] 0.50

llaveInglesa :: Herramienta
llaveInglesa = Herramienta "Llave Inglesa" 200 Hierro

martillo :: Herramienta
martillo = Herramienta "Martillo" 20 Madera

llaveFrancesa :: Herramienta
llaveFrancesa = Herramienta "Llave Francesa" 1 Hierro

destornillador :: Herramienta
destornillador = Herramienta "Destornillador" 0 Plastico

aumentarPrecio :: Float -> Herramienta -> Herramienta
aumentarPrecio unValor unaHerramienta = unaHerramienta {precio = precio unaHerramienta + unValor}

--2
--a
tieneHerramienta :: Herramienta -> Plomero -> Bool
tieneHerramienta unaHerramienta unPlomero = unaHerramienta `elem` cajaDeHerramientas unPlomero

--b 
esMalvado :: Plomero -> Bool
esMalvado unPlomero =  take 2 (nombre unPlomero) == "Wa"

--c
puedeComprar :: Herramienta -> Plomero -> Bool
puedeComprar unaHerramienta unPlomero = dinero unPlomero >= precio unaHerramienta

--3
herramientaEsBuena :: Herramienta -> Bool
herramientaEsBuena unaHerramienta =
    tieneEmpuniaduraHierro ||
    denominacion unaHerramienta == "Martillo" &&
    saberMaterialHerramienta Madera unaHerramienta || saberMaterialHerramienta Goma unaHerramienta
    where
        tieneEmpuniaduraHierro = precio unaHerramienta >= 10000

saberMaterialHerramienta :: Material -> Herramienta -> Bool
saberMaterialHerramienta unMaterial unaHerramienta = materialEmpuniadura unaHerramienta == unMaterial

--4
comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta unaHerramienta unPlomero
    | puedeComprar unaHerramienta unPlomero = agregarHerramienta unaHerramienta . modificarDinero (precio unaHerramienta) $ unPlomero
    | otherwise = unPlomero

agregarHerramienta :: Herramienta -> Plomero -> Plomero
agregarHerramienta unaHerramienta unPlomero = unPlomero { cajaDeHerramientas = unaHerramienta : cajaDeHerramientas unPlomero }

modificarDinero :: Float -> Plomero -> Plomero
modificarDinero valor unPlomero = unPlomero {dinero = dinero unPlomero + valor}

--5
--a
data Reparacion = Reparacion {
    descripcion :: String,
    requerimiento :: Requerimiento
} deriving (Show)

type Requerimiento = Plomero ->  Bool

filtracionDeAgua :: Reparacion
filtracionDeAgua = Reparacion "Filtracion de Agua" (tieneHerramienta llaveInglesa)

--b
esReparacionDificil :: Reparacion -> Bool
esReparacionDificil unaReparacion = descripcionComplicada && esUnGrito unaReparacion
    where
        descripcionComplicada = longitudDescripcion unaReparacion > 100

esUnGrito :: Reparacion -> Bool
esUnGrito unaReparacion = all (`elem` ['A'..'Z']) (descripcion unaReparacion)

--c 
presupuestoReparacion :: Reparacion -> Float
presupuestoReparacion unaReparacion =
    fromIntegral (longitudDescripcion unaReparacion * 3)

longitudDescripcion :: Reparacion -> Int
longitudDescripcion unaReparacion = length (descripcion unaReparacion)

--6
hacerUnaReparacion :: Reparacion -> Plomero -> Plomero
hacerUnaReparacion unaReparacion unPlomero
    | puedeReparar unaReparacion unPlomero = (evaluarMalvado unaReparacion ). (agregarReparacion unaReparacion) . modificarDinero (presupuestoReparacion unaReparacion) $ unPlomero
    | otherwise                            = modificarDinero 100 unPlomero

puedeReparar :: Reparacion -> Plomero -> Bool
puedeReparar unaReparacion unPlomero= 
    (requerimiento unaReparacion) unPlomero ||
    (esMalvado unPlomero && tieneHerramienta martillo unPlomero)


agregarReparacion :: Reparacion -> Plomero -> Plomero
agregarReparacion unaReparacion unPlomero = unPlomero {historialReparaciones = unaReparacion : historialReparaciones unPlomero}

evaluarMalvado :: Reparacion -> Plomero -> Plomero
evaluarMalvado unaReparacion unPlomero 
    | esMalvado unPlomero = agregarHerramienta destornillador unPlomero
    | not (esMalvado unPlomero) && esReparacionDificil unaReparacion = perderBuenas unPlomero
    | otherwise = perderUnaHerramienta unPlomero


perderBuenas :: Plomero -> Plomero
perderBuenas unPlomero = unPlomero {cajaDeHerramientas = filter (not . herramientaEsBuena) (cajaDeHerramientas unPlomero)}

perderUnaHerramienta :: Plomero -> Plomero
perderUnaHerramienta unPlomero = unPlomero {cajaDeHerramientas = drop 1 (cajaDeHerramientas unPlomero)}

--7
jornadaTrabajo :: Plomero -> [Reparacion] -> Plomero
jornadaTrabajo unPlomero unasReparaciones = foldl (flip hacerUnaReparacion) unPlomero unasReparaciones

--8
--a
jornadaTrabajoEmpleados :: [Plomero] -> [Reparacion] -> [Plomero]
jornadaTrabajoEmpleados unosPlomeros unasReparaciones = map (flip jornadaTrabajo unasReparaciones) unosPlomeros

masReparador :: [Plomero] -> [Reparacion] -> Plomero
masReparador unosPlomeros unasReparaciones = 

{-
compararPlomeros :: Comparador -> [Plomero] -> Plomero
compararPlomeros comparador [x] = x
compararPlomeros comparador (x:y:xs) 
    |comparador x > comparador y = compararPlomeros comparador (x:xs) 
    |otherwise = compararPlomeros comparador (y:xs) 
-}


-- 1 --
data Obra = Obra {
    titulo :: String,
    anioPublicacion :: Int
} deriving (Show)

data Autor = Autor{
    nombreAutor :: String,
    obras :: [Obra]
} deriving (Show)

Obra1 :: Obra
Obra1 = Obra "Había una vez un pato" 1997

Obra2:: Obra
Obra2 = Obra "¡Habia una vez un pato!" 1996

Obra3 :: Obra
Obra3 = Obra "Mirtha, Susana y Moria" 2010

Obra4 :: Obra 
Obra4 = Obra "La semántica funcional del amoblamiento vertebral es riboeficiente" 2020

Obra5 :: Obra
Obra = Obra "La semántica funcional de Mirtha, Susana, y Moria" 2022

Juan :: Autor
Juan = Autor "Juan Perez" [Obra1, Obra3]

Franco :: Autor
Franco = Autor "Franco Serrano" [Obra5]

Eusebio :: Autor
Eusebio = Autor "Eusebio Insua" [Obra4]

-- 2 --
versionCruda :: String -> String
versionCruda textoSinModificar textoConAcentosYPuntuacion = map sacarAcentos . (filter ( not. sacarPuntuacion) textoConAcentosYPuntuacion )

sacarAcentos :: Char -> Char
sacarAcentos "á" = "a"
sacarAcentos "é" = "e"
sacarAcentos "í" = "i"
sacarAcentos "ó" = "o"
sacarAcentos "ú" = "u"
sacarAcentos _ = _

sacarPuntuacion :: Char -> Bool
sacarPuntuacion "!" = True
sacarPuntuacion "¡" = True
sacarPuntuacion "," = True
sacarPuntuacion "." = True
sacarPuntuacion _ = False


-- 3 -- 
copiaLiteral :: Obra -> Obra -> Bool
copiaLiteral obra1 obra2 = versionCruda ( titulo obra1 ) == versionCruda ( titulo obra2 )

empiezaIgual :: Int -> Obra -> Obra -> Bool
empiezaIgual n obra1 obra2 =  take n (titulo obra1) == take n (titulo obra2) && length (titulo obra1) < length (titulo obra2)


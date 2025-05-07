module Library where
import PdePreludat

data Obra = UnaObra {
    contenido :: String,
    fecha :: Number
} deriving (Show, Eq)

data Autor = UnAutor{
   nombre :: String,
   obras :: [Obra]
} deriving (Show,Eq)

obraA :: Obra
obraA = UnaObra "Había una vez un pato." 1997
--B
obraB :: Obra
obraB = UnaObra "¡Habia una vez un pato!" 1998
--C
obraC :: Obra
obraC = UnaObra "Mirtha, Susana y Moria." 2010
--D
obraD :: Obra
obraD = UnaObra "La semántica funcional del amoblamiento vertebral es riboficiente" 2020
--E
obraE :: Obra
obraE = UnaObra "La semántica funcional de Mirtha, Susana y Moria." 2022

autor1 :: Autor
autor1 = UnAutor "nn1" [obraA]
autor2 :: Autor
autor2 = UnAutor "nn2" [obraB, obraC]
autor3 :: Autor
autor3 = UnAutor "nn3" [obraB, obraD]
autor4 :: Autor
autor4 = UnAutor "nn4" [obraE, obraB]

versionCruda :: String -> String
versionCruda = soloAlfanumericos.sinAcentos

sinAcentos :: String -> String
sinAcentos = map sacarAcento

sacarAcento :: Char -> Char
sacarAcento 'á' = 'a'
sacarAcento 'é' = 'e'
sacarAcento 'í' = 'i'
sacarAcento 'ó' = 'o'
sacarAcento 'ú' = 'u'
sacarAcento 'Á' = 'A'
sacarAcento 'É' = 'E'
sacarAcento 'Í' = 'I'
sacarAcento 'Ó' = 'O'
sacarAcento 'Ú' = 'U'
sacarAcento letra = letra

soloAlfanumericos :: String -> String
soloAlfanumericos = filter esLetraONumero

esLetraONumero :: Char -> Bool
esLetraONumero caracter = elem caracter caracteresValidos

caracteresValidos :: [Char]
caracteresValidos = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "


distanciaHamming :: String -> String -> Number
distanciaHamming [] [] = 0
distanciaHamming (x:xs) (y:ys) 
                                | x /= y = 1 + distanciaHamming xs ys
                                | otherwise = distanciaHamming xs ys



-- Punto 4
type FormaDeteccion = Obra -> Obra -> Bool

esPlagioSegun :: Obra -> Obra -> FormaDeteccion -> Bool
esPlagioSegun plagio original deteccion = esPosterior plagio original && deteccion plagio original

esPosterior :: Obra -> Obra -> Bool
esPosterior plagio original = fecha original < fecha plagio

-- forma 1
esCopiaLiteral :: FormaDeteccion
esCopiaLiteral plagio original = versionCruda (contenido plagio) == versionCruda (contenido original)

-- forma 2
empiezaIgual :: Number -> FormaDeteccion
empiezaIgual cantidad plagio original = length (contenido plagio) > length (contenido original) && primerasNLetrasIguales cantidad plagio original

primerasNLetrasIguales :: Number -> Obra -> Obra -> Bool
primerasNLetrasIguales cantidadDeCaracteres plagio original = primerasNLetras cantidadDeCaracteres plagio == primerasNLetras cantidadDeCaracteres original

primerasNLetras :: Number -> Obra -> String
primerasNLetras cantidadDeCaracteres obra = take cantidadDeCaracteres (contenido obra)

-- forma 3

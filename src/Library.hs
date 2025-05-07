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

-- forma 3 (Puede que este alrevez los parametros no lo cambio xq sino le mato la consola xD)

leAgregaronIntro :: FormaDeteccion
leAgregaronIntro original plagio = ultimosNCaracteresAlrevez ((length . contenido) original) (contenido plagio) ==  (reverse . contenido) plagio

ultimosNCaracteresAlrevez :: Number -> String -> String
ultimosNCaracteresAlrevez numero = take numero . reverse

--- forma 4 (-- forma 3 (Puede que este alrevez los parametros no lo cambio xq sino le mato la consola xD)

distanciaDeHammingMenorA :: Number -> FormaDeteccion
distanciaDeHammingMenorA numero original plagio = distanciaHamming (contenido original) (contenido plagio) > numero


-- Punto 5

-- Modelar dos bots de ejemplo, incluyendo todas las formas de detección existentes hasta ahora.

data Bot = UnBot {
    formas :: [FormaDeteccion],
    fabricante :: String
} deriving (Show)

botSimple :: Bot
botSimple = UnBot [esCopiaLiteral] "Fabricante A"

botCompleto :: Bot
botCompleto = UnBot [esCopiaLiteral, empiezaIgual 100, leAgregaronIntro, distanciaDeHammingMenorA 3] "Super Fabricante"


-- Un bot detecta si una obra es plagio de otra si verifica alguna de las formas de detección que maneja.

detectarPlagio :: Bot -> Obra -> Obra -> Bool
detectarPlagio bot plagio original = any (esPlagioSegun plagio original) (formas bot)

-- detectarPlagio bot plagio original = any (detecta plagio original) (formas bot)
-- detecta plagio original forma = esPlagioSegun plagio original forma


-- Dado un conjunto de autores y un bot, detectar si es una cadena de plagiadores. Es decir, el segundo plagió al primero, el tercero al segundo, y así. Se considera que un autor plagió a otro cuando alguna de sus obras es plagio de alguna de las del otro según el bot.

esCadena :: [Autor] -> Bot -> Bool
esCadena (plagiado:plagiador:resto) bot = plagioAutor bot plagiador plagiado && esCadena (plagiador:resto) bot
esCadena _ _ = True

plagioAutor :: Bot -> Autor -> Autor -> Bool
plagioAutor bot plagiador plagiado = any (esPlagioDeAlguna bot (obras plagiado)) (obras plagiador)

esPlagioDeAlguna :: Bot -> [Obra] -> Obra -> Bool
esPlagioDeAlguna bot originales plagio = any (detectarPlagio bot plagio) originales

-- Dado un conjunto de autores y un bot, encontrar a los autores que  "hicieron plagio pero aprendieron",  que significa que luego de que el bot detectara que una de sus obras fue plagio de alguna de los otros autores, nunca más volvió a plagiar. En definitiva, su plagio detectado fue el primero y el último.

{-
aprendieron :: [Autor] -> Bot -> [Autor]
aprendieron autores bot = filter (aprendio autores bot) autores

aprendio :: [Autor] -> Bot -> Autor -> Bool
aprendio autores bot = (1==) . length . losPlagios autores bot

losPlagios :: [Autor] -> Bot -> Autor -> [Obra]
losPlagios autores bot elQueAprendio = filter (detectarPlagio bot plagio) (autores)

aprendieron :: Bot -> [Autor] -> [Autor]
aprendieron bot autores = filter (\a -> aprendio bot a (quitar a autores)) autores 

quitar :: Eq a => a -> [a] -> [a]
quitar x = filter (/= x)

aprendio :: Bot -> Autor -> [Autor] -> Bool
aprendio bot autor autores =  length (obrasPlagiadasDelAutor bot autor autores) == 1

obrasPlagiadasDelAutor :: Bot -> Autor -> [Autor] -> [Obra]
obrasPlagiadasDelAutor bot autor autores =  filter (esPlagioDeAlgunoDeEstosAutores bot autores) (obras autor) 

esPlagioDeAlgunoDeEstosAutores :: Bot -> [Autor] -> Obra -> Bool
esPlagioDeAlgunoDeEstosAutores  bot autores obra = any (\autor -> esPlagioDeEsteAutor bot autor obra) autores
-}


--9a)------------------------------------
-- Codificar una obra infinita.

aesInfinitas :: String
aesInfinitas = 'a' : aesInfinitas

besInfinitas' :: String
besInfinitas' = cycle "b"

obraInfinita :: Obra
obraInfinita = UnaObra aesInfinitas 2021

obraInfinita2 :: Obra
obraInfinita2 = UnaObra aesInfinitas 2025

--9b---------------------------------------------------
-- ¿Qué sucede si se desea verificar si esa obra es plagio de otra con cada una de las formas existentes? Justificar conceptualmente en cada caso.

-- Una consulta como esPlagioSegun obraInfinita obraInfinita2 esCopiaLiteral
-- Independientemente de que tenga contenidos infinitos, me va a dar False, porque ya no se cumple
-- que las fechas sean posteriores. Por lazy evaluation, mirando el código de esPlagioSegun, no necesita evaluar la forma de detección:
-- esPlagioSegun plagio original deteccion = esPosterior plagio original && deteccion plagio original

-- Qué pasa si sí son posteriores, ahí sí debe verificar las formas:

-- En cada caso:

-- esPlagioSegun obraInfinita obraA esCopiaLiteral
-- Para esCopiaLiteral, necesita verificar en algún momento:
-- versionCruda aesInfinitas == versionCruda "Había una vez un pato."
-- Por lazy evaluation, no necesita comparar todo el string para darse cuenta que es falso.

-- El caso interesante es con: esPlagioSegun obraInfinita2 obraInfinita esCopiaLiteral
-- Acá se cuelga. Independendientemente de que exista lazy evaluation, necesito chequear todo el string para saber si es igual. En el caso anterior, con una letra distinta bastaba para dar False. Acá nunca termina. Se cuelga.

-- esPlagioSegun obraInfinita obraA empiezaIgual
-- Seguro se cuelga, porque se cuelga el length del contenido. 

-- esPlagioSegun obraInfinita obraA leAgregaronIntro
-- Se cuelga siempre por la razón anterior (el length)

-- esPlagioSegun obraInfinita obraA distanciaHammingMenorA
-- Se cuelga porque necesito verificar todo el string para conocer la distancia de hamming, y acá lazy evaluation no me puede salvar.
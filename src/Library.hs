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

--Punto 1
-- A
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

--Punto 2 (10')

-- ? En que consiste la version cruda? es una version donde se eliminan los acentos y se quitan los caracteres no alfanuméricos.
-- ? Hay un orden de preferencia? Si, porque si saco los no alfanuméricos primero, me vuela las que tienen tilde.
versionCruda :: String -> String
versionCruda = soloAlfanumericos.sinAcentos

-- ? Como saco los acentos? transformo las letras con acento en letras sin acento (map)
sinAcentos :: String -> String
sinAcentos = map sinAcento

-- ? Como reemplazo una letra con acento por una sin? (pattern matching)
sinAcento :: Char ->  Char
sinAcento 'á' = 'a'
sinAcento 'é' = 'e'
sinAcento 'í' = 'i'
sinAcento 'ó' = 'o'
sinAcento 'ú' = 'u'
sinAcento 'Á' = 'A'
sinAcento 'É' = 'E'
sinAcento 'Í' = 'I'
sinAcento 'Ó' = 'O'
sinAcento 'Ú' = 'U'
sinAcento letra = letra

-- ? Como saco las letras no alfanumericas? tengo que volarlas al choto, o mejor dicho, quedarme solo con las alfanumericas (filter)
soloAlfanumericos :: String -> String
soloAlfanumericos = filter esLetraONumero

-- ? Que significa qeu sea alfanumerico? Es una letra o un numero, es decir, si pertenece a una lista que corresponde a esas caracteristicas
esLetraONumero :: Char ->  Bool
esLetraONumero caracter = elem caracter todasLasLetrasYNumeros 

-- ? Cuales son las letras y numeros? En realidad es falso esto porque le sumamos el espacio
todasLasLetrasYNumeros :: [Char]
todasLasLetrasYNumeros = ['a'..'z']++['A'..'Z'] ++ "0123456789" ++ " "


--Punto 3 (15')

-- ? Que es la distancia de Hamming? Representa la diferencia que hay de letras entre dos palabras

-- ? Como puedo averiguar esto? Necesito comparar cada letra y ver si son iguales o distintas. en caso de ser distintas, la distancia de hamming incrementa en uno

-- dist "hola" "pera" = 3 = dist "h" "p" + dist "ola" "era" = 1 + 2
-- dist "ola" "era" = 2 = dist "o" "e" + dist "la" "ra" = 1 + 1
-- dist "la" "ra" = 1 = dist "l" "r" + dist "a" "a" = 1 + 0
-- dist "a" "a" = 0 = dist "a" "a" + dist "" "" = 0 + ???

-- *distanciaHamming :: String -> String -> Number
-- *distanciaHamming (x:xs) (y:ys) |x /= y = distanciaHamming xs ys + 1
-- *                               |otherwise = distanciaHamming xs ys

-- ? Que onda con la distancia entre dos cadenas vacias? Si se puede hacer, seria una secuencia infinita ya que 
-- dist "" "" = dist "" "" + dist "" "" = ??? + ???

-- Como corto esto? Necesito poner una condicion de corte (caso base):
-- dist "" "" = 0

-- De esta manera
-- dist "a" "a" = 0 = dist "a" "a" + dist "" "" = 0 + 0
-- *distanciaHamming [] [] = 0


distanciaHamming :: String -> String -> Number
distanciaHamming [] [] = 0
distanciaHamming (x:xs) (y:ys) |x /= y = distanciaHamming xs ys + 1
                               |otherwise = distanciaHamming xs ys



--plagios 

--Punto 4
-- ? Que es un plagio? Es una copia de una obra. Para que una obra plagie a otra, esta debe ser posterior.
-- *esPlagio :: Obra -> Obra -> Bool
-- *esPlagio plagio original = esPosterior plagio original && ???

-- ? Hay distintas formas de plagio. Que son esas formas? Son funciones que detectan plagios. Son formas de deteccion. No me importa ahora mismo como funcionan, solo se que hacen.

-- ? Entonces como queda esPlagio? es plagio si huele a plagio y la plagiadora es posterior a la original

-- * esPlagio :: Obra -> Obra -> FormaDeteccion -> Bool
-- * esPlagio plagio original deteccion = esPosterior plagio original  && ...
-- ? Que mas? Hay algo raro, huele.. (huele a plagio)

-- ? Que significa que huele a plagio?
hueleAPlagio :: Obra -> Obra -> FormaDeteccion -> Bool
hueleAPlagio plagio original deteccion = deteccion (contenido plagio) (contenido original)

-- ?Como modelo una deteccion? Todas se basan en los titulos y me dicen si es un plagio o no
-- *
type FormaDeteccion = String ->  String ->  Bool
-- *
-- ? Que significa que sea posterior? que la fecha del plagio es mayor a la de la original
esPosterior :: Obra -> Obra -> Bool
esPosterior plagio original = fecha plagio > fecha original


esPlagio :: Obra -> Obra -> FormaDeteccion -> Bool
esPlagio plagio original deteccion =  esPosterior plagio original && hueleAPlagio plagio original deteccion

-- esPlagio :: Obra -> Obra -> FormaDeteccion -> Bool
-- esPlagio plagio obraOriginal deteccion = fecha plagio > fecha obraOriginal && deteccion (contenido plagio) (contenido obraOriginal)  

--a
copiaLiteral :: FormaDeteccion
copiaLiteral texto textoOriginal = versionCruda texto == versionCruda textoOriginal
--b

empiezaIgual :: Number ->  FormaDeteccion
empiezaIgual cantidadDeCaracteres texto textoOriginal = take cantidadDeCaracteres texto == take cantidadDeCaracteres textoOriginal &&  length texto < length textoOriginal
--c
leAgregaronIntro :: FormaDeteccion
leAgregaronIntro texto textoOriginal = ultimosElementos (length textoOriginal)  texto == textoOriginal

ultimosElementos :: Number -> String -> String
ultimosElementos cant texto  = drop (length texto - cant) texto
--d olvidable

-- 
--punto 5
data Bot = UnBot {
    formas :: [FormaDeteccion],
    fabricante :: String
} 

botA :: Bot
botA = UnBot [copiaLiteral, leAgregaronIntro, empiezaIgual 10] "botter"

botB :: Bot
botB = UnBot [empiezaIgual 10, leAgregaronIntro] "botter"

--6. Un bot detecta si una obra es plagio de otra si verifica alguna de las formas de detección que maneja.

esPlagioDeEstaObra :: Bot -> Obra -> Obra -> Bool
esPlagioDeEstaObra bot obra obraOriginal = any (esPlagio obra obraOriginal)  (formas bot)

--7. Dado un conjunto de autores y un bot, detectar si es una cadena de plagiadores. Es decir, el segundo plagió al primero, el tercero al segundo, y así. Se considera que un autor plagió a otro cuando alguna de sus obras es plagio de alguna de las del otro según el bot.

cadenaPlagiadores :: Bot ->  [Autor] -> Bool
cadenaPlagiadores bot [_] = False
cadenaPlagiadores bot [x1,x2] = plagioA bot x1 x2
cadenaPlagiadores bot (x1:x2:xs) = plagioA bot x1 x2 && cadenaPlagiadores bot (x2:xs)

plagioA :: Bot ->  Autor ->  Autor -> Bool
plagioA bot autor autorOriginal = any (esPlagioDeEsteAutor bot autorOriginal) (obras autor)

esPlagioDeEsteAutor :: Bot -> Autor ->  Obra -> Bool
esPlagioDeEsteAutor bot autorOriginal obra = any (esPlagioDeEstaObra bot obra) (obras autorOriginal)

--plagioA bot autor autorOriginal = any (\obra -> any (esPlagioDeEstaObra bot obra) (obras autorOriginal)) (obras autor)

-- 8. Dado un conjunto de autores y un bot, encontrar a los autores que  "hicieron plagio pero aprendieron",  que significa que luego de que el bot detectara que una de sus obras fue plagio de alguna de los otros autores, nunca más volvió a plagiar. En definitiva, su plagio detectado fue el primero y el último.

aprendieron :: Bot -> [Autor] -> [Autor]
aprendieron bot autores = filter (\a -> aprendio bot a (quitar a autores)) autores 
  where quitar x = filter (/= x)

aprendio :: Bot -> Autor -> [Autor] -> Bool
aprendio bot autor autores =  length (obrasPlagiadasDelAutor bot autor autores) == 1

obrasPlagiadasDelAutor :: Bot -> Autor -> [Autor] -> [Obra]
obrasPlagiadasDelAutor bot autor autores =  filter (esPlagioDeAlgunoDeEstosAutores bot autores) (obras autor) 

esPlagioDeAlgunoDeEstosAutores :: Bot -> [Autor] -> Obra -> Bool
esPlagioDeAlgunoDeEstosAutores  bot autores obra = any (\autor -> esPlagioDeEsteAutor bot autor obra) autores

-- 9a.
obraInfinita :: Obra
obraInfinita = UnaObra (repeat 'a') 2021

obraInfinita2 :: Obra
obraInfinita2 = UnaObra (repeat 'a') 2025

-- 9b.
{-

Suponiendo una consulta como: esPlagio obraA obraInfinita copiaLiteral
No importa cuál sea la forma de detección, como la fecha de la obra que se pregunta si es plagio es anterior, no se cumple y corta diciendo False, por Lazy evaluation no es necesario seguir evaluando el otro lado del &&.

Ahora veamos los casos donde se cumple que la fecha es posterior:

copiaLiteral:
- Suponiendo la consulta: esPlagio obraInfinita obraA copiaLiteral
da False, por Lazy Evaluation. Al evaluar la igualdad de contenidos no necesita evaluar toda la lista infinita para saber que los strings son distintos, con los primeros caracteres alcanza.
- Pero si consulto esPlagio obraInfinita2 obraInfinita copiaLiteral, se cuelga, porque para saber si dos strings iguales son iguales necesita recorrerlos todos, aún con lazy evaluation.

empiezaIgual:
- Suponiendo la consulta: esPlagio obraInfinita obraA empiezaIgual
Entonces da False, pues verifica con take que sean iguales los contenidos y eso da false. Por Lazy evaluation no es necesario evaluar la lista infinita para el take.
- Suponiendo la consulta: esPlagio obraInfinita2 obraInfinita empiezaIgual
Ahí se cuelga, porque nunca llega a comparar las longitudes de los contenidos, aún con lazy evaluation. Es decir, aún si una es infinita y la otra empieza igual, jamás cortará.

leAgregaronIntro:
- Suponiendo la consulta: esPlagio obraInfinita obraA leAgregaronIntro
Aún teniendo lazy evaluation, para el calcular el length del contenido de la obra infinita se cuelga, antes de poder hacer el drop.
- Ahora, si hacemos al revés: esPlagio obraA obraInfinita leAgregaronIntro
Se colgaría, pues se pide hacer un ultimosElementos, que a su vez necesita el length de la lista infinita, no hay Lazy evaluation que lo salve.
-}
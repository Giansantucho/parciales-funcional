module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--Estructuras

data Persona = Persona {
  nombrePersona :: String,
  suerte :: Number,
  inteligencia :: Number,
  fuerza :: Number
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)

----1----

niveles :: Persona -> [Number]
niveles persona = [fuerza persona, suerte persona, inteligencia persona]

sumaDeNiveles :: Persona -> Number
sumaDeNiveles = sum.niveles

diferenciaDeNiveles :: Persona -> Number
diferenciaDeNiveles persona = (maximum.niveles) persona - (minimum.niveles) persona

nivelesMayoresA :: Number -> (Persona -> Number)
nivelesMayoresA n = length.filter(>n).niveles 

---2---

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion = concat.map efectos.ingredientes --el concat me aplana la lista de listas [[],[]] => []

---3---

pocionesHardcore :: [Pocion] -> [String]
pocionesHardcore = map nombrePocion . filter ((>=4).length.efectosDePocion)

cantPocionesProhibidas :: [Pocion] -> Number
cantPocionesProhibidas = length.filter esProhibida

esProhibida :: Pocion -> Bool
esProhibida = any (flip elem nombresDeIngredientesProhibidos.nombreIngrediente).ingredientes

sonTodasDulces :: [Pocion] -> Bool
sonTodasDulces = all (any (("azucar"==).nombreIngrediente).ingredientes)

---4---
tomarPocion :: Pocion -> Persona -> Persona
tomarPocion pocion persona = (foldl (\per efecto -> efecto per) persona .efectosDePocion) pocion

---5---

esAntidotoDe :: Persona -> Pocion -> Pocion -> Bool
esAntidotoDe p1 p2 persona = ((==persona).tomarPocion p2 . tomarPocion p1) persona}

--6--

personaMasAfectada :: Pocion -> (Persona -> Number) -> ([Persona] -> Persona)
personaMasAfectada pocion criterio = maximoSegun (criterio.tomarPocion pocion)

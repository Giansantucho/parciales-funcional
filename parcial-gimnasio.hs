module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

----Tipos de dato

type Peso = Number
type Tiempo = Number
type Grados = Number

data Gimnasta = Gimnasta{
     peso :: Number,
     tonificacion :: Number
} deriving (Show,Eq)

data Rutina = Rutina {
  nombre :: String,
  duracionTotal :: Tiempo,
 ejercicios :: [Ejercicio]
}

--1--

tonificar :: Number -> Gimnasta -> Gimnasta
tonificar n gimnasta = gimnasta{tonificacion = tonificacion gimnasta + n}

quemarCalorias :: Number -> Gimnasta -> Gimnasta
quemarCalorias kcal gimnasta = gimnasta{peso = peso gimnasta - kcal `div` 500 }

---2--

type Ejercicio = Tiempo -> (Gimnasta -> Gimnasta)

--a--

cinta :: Number -> Ejercicio
cinta vel tiempo = quemarCalorias (tiempo * vel * 10)

--a.i--

caminata :: Ejercicio
caminata = cinta 5 

--a.ii-

pique :: Ejercicio
pique tiempo = cinta (tiempo `div` 2 + 20) tiempo

--b--

pesas :: Peso -> Ejercicio
pesas kg tiempo | tiempo > 10 = tonificar kg
                | otherwise = id

colina :: Grados -> Ejercicio
colina inclinacion tiempo = quemarCalorias(2*tiempo*inclinacion) 

montaña :: Grados -> Ejercicio
montaña inclinacion tiempo = tonificar 3.colina (inclinacion + 5) (tiempo `div` 2).colina inclinacion (tiempo `div` 2)

--3--

realizarRutina :: Gimnasta -> Rutina ->  Gimnasta
realizarRutina gimnastaInicial rutina = foldl (\gimnasta ej -> ej (tiempoParaEjercicio rutina) gimnasta) gimnastaInicial (ejercicios rutina)

tiempoParaEjercicio :: Rutina -> Tiempo
tiempoParaEjercicio rutina = ((div (duracionTotal rutina)).length.ejercicios) rutina

ejemplo = realizarRutina (Gimnasta 90 0) (Rutina "Mi rutina" 30 [caminata, pique, pesas 5, colina 30, montaña 30])

--4--

mayorCantEjercicios :: [Rutina] -> Number
mayorCantEjercicios = maximum . map (length.ejercicios) 

nombreRutinaTonificacion :: Gimnasta -> ([Rutina] ->[String])
nombreRutinaTonificacion gimnasta = map nombre . filter ((>tonificacion gimnasta).tonificacion.realizarRutina gimnasta)

hayPeligrosas :: Gimnasta -> ([Rutina] -> Bool)
hayPeligrosas gimnasta = any ((< peso gimnasta `div` 2).peso.(realizarRutina gimnasta))
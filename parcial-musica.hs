module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Cancion = [Nota]

data Nota = Nota {
tono :: Number, -- Frecuencia medida en Hz
volumen :: Number, -- Volumen de reproducción medido en Db
duracion :: Number -- Tiempo de reproducción medido en segundos
} deriving (Eq, Show)

-- FUNCIONES AUXILIARES
cambiarVolumen :: (Number -> Number) -> Nota -> Nota
-- Dada una función transformación y una nota retorna una copia de la
-- nota con el volumen igual al resultado de aplicar la transformación a
-- su volumen actual.
cambiarVolumen delta nota = nota { volumen = delta (volumen nota) }
cambiarTono :: (Number -> Number) -> Nota -> Nota
-- Dada una función transformación y una nota retorna una copia de la
-- nota con el tono igual al resultado de aplicar la transformación a
-- su tono actual.
cambiarTono delta nota = nota { tono = delta (tono nota) }
cambiarDuracion :: (Number -> Number) -> Nota -> Nota
-- Dada una función transformación y una nota retorna una copia de la
-- nota con la duración igual al resultado de aplicar la transformación a
-- su duración actual.
cambiarDuracion delta nota = nota { duracion = delta (duracion nota) }
promedio :: [Number] -> Number
-- Dada una lista de números retorna el valor promedio
promedio lista = sum lista / (length lista)

---1---

between :: Number -> Number -> Number -> Bool
between n1 n2 valor = valor >= n1 && valor <= n2

esAudible :: Nota -> Bool
esAudible nota = volumen nota > 10 && between 20 20000 (tono nota)

esMolesta :: Nota -> Bool
esMolesta nota = condicionMolesta1 nota || condicionMolesta2 nota

condicionMolesta1 :: Nota -> Bool
condicionMolesta1 nota = esAudible nota && tono nota < 250 && volumen nota > 85

condicionMolesta2 :: Nota -> Bool
condicionMolesta2 nota = esAudible nota && tono nota >=250 && volumen nota > 55

--2--

silencioTotal :: Cancion -> Number
silencioTotal  = sum.map duracion.filter (not.esAudible) 

sinInterrupciones :: Cancion -> Bool
sinInterrupciones = all(esAudible).filter (\nota -> duracion nota > 0.1)

peorMomento :: Cancion -> Number
peorMomento = maximum.map volumen.filter (esMolesta)

--3--

type Filtro = Cancion -> Cancion

trasponer :: Number -> Filtro
trasponer n = map (cambiarTono (*n))

acotarVolumen :: Number -> Number -> Filtro
acotarVolumen maximo minimo = map(cambiarVolumen (max minimo).cambiarVolumen (min maximo))

normalizar :: Filtro
normalizar cancion = 
    map ( cambiarVolumen (\ _ -> volumenPromedio) ) $cancion
    where volumenPromedio = promedio . map volumen $cancion


--5--
tunear :: Cancion -> [Filtro] -> Cancion
tunear cancion =
    normalizar . foldr ($) cancion
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Serie = Serie{
    nombre :: String,
    actores :: [Actor],
    presupuesto :: Number,
    temporadas :: Number,
    rating :: Number,
    cancelada :: Bool
} deriving (Show,Eq)

data Actor = Actor{
    nombreActor :: String,
    sueldoPretendido :: Number,
    restricciones :: [String]
} deriving (Show,Eq)

--1--

estaEnRojo :: Serie -> Bool
estaEnRojo serie =  presupuesto serie < (cobranActores (actores serie))

cobranActores :: [Actor] -> Number
cobranActores = (sum.map sueldoPretendido) 

esProblematica :: Serie -> Bool
esProblematica  = (> 3).cantidadDeActoresConMasDeUnaRestriccion 

cantidadDeActoresConMasDeUnaRestriccion :: Serie -> Number
cantidadDeActoresConMasDeUnaRestriccion = length.filter (tienenMasDeUnaRestriccion).actores 

tienenMasDeUnaRestriccion :: Actor -> Bool
tienenMasDeUnaRestriccion  = (>1).length.restricciones


--2--

type Produccion = Serie -> Serie

conFavoritismos :: [Actor] -> Produccion
conFavoritismos actoresFavoritos = reemplazarActores actoresFavoritos.eliminarActores 2

eliminarActores ::Number -> Produccion
eliminarActores num serie = serie{
    actores = drop num (actores serie)
}

reemplazarActores :: [Actor] -> Produccion
reemplazarActores actoresFavs serie = serie{
    actores = (++ actoresFavs) $ actores serie
}

---
timBurton :: Produccion
timBurton = conFavoritismos [johnnyDepp,helena]

johnnyDepp = Actor "johnny depp" 20000000 []
helena = Actor "helema bonham carter" 15000000 []

---
gatopardeitor :: Produccion
gatopardeitor = id

serieDePrueba = Serie "Serie" [johnnyDepp] 150000000 4 1.5 False

---
estireitor :: Produccion
estireitor serie = serie{
    temporadas = (*2) $ temporadas serie
}

desespereitor :: Produccion -> Produccion -> Produccion 
desespereitor prod1 prod2 = prod1.prod2

canceleitor :: Number -> Produccion
canceleitor cifra serie | estaEnRojo serie || rating serie <= cifra = serie {cancelada = True}
                        | otherwise = serie

----3----

bienestarSerieLongitud :: Serie -> Number
bienestarSerieLongitud serie | temporadas serie > 4 = 5
                             | otherwise = (10 - temporadas serie) * 2

bienestarSerieActores :: Serie -> Number
bienestarSerieActores serie | ((<10).length.actores) serie = 3
                            | otherwise = max (10 - (length.filter (tienenMasDeUnaRestriccion).actores) serie) 2

bienestarSerie :: Serie -> Number
bienestarSerie serie | cancelada serie = 0
                     | otherwise = bienestarSerieActores serie + bienestarSerieLongitud serie     

---4---

productorMasEfectivo :: [Serie] -> [Produccion] -> [Serie]
productorMasEfectivo series productores = map (esMasEfectivo productores) series

esMasEfectivo :: [Produccion] -> Serie -> Serie
esMasEfectivo (x:[]) serie = x serie 
esMasEfectivo (x:xs) serie
  | bienestarSerie (x serie) > bienestarSerie (head xs $ serie) = x serie
  | otherwise = esMasEfectivo xs serie

--6--

esControvertida :: Serie -> Bool
esControvertida serie = not $ cobraMasQueElSiguiente (actores serie)

cobraMasQueElSiguiente :: [Actor] -> Bool
cobraMasQueElSiguiente (x:[]) = True
cobraMasQueElSiguiente (x:xs) = (sueldoPretendido x) > (sueldoPretendido $ head xs) 
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


----Primera Parte

----Estructuras iniciales

--1

type Habilidad = String

data Personaje = UnPersonaje{
  edad :: Number,
  energia :: Number,
  habilidades :: [Habilidad],
  nombre :: String,
  planeta :: String
} deriving (Show,Eq)



data Guantelete = Guantelete{
  material :: String,
  gemas :: [Gema]
} deriving (Show,Eq)

type Universo =  [Personaje]


chasquear :: Guantelete -> Universo -> Universo
chasquear guante universo | ((== 6).length.gemas) guante  && (("uru" ==).material) guante = take ((length universo) `div` 2) universo
                          | otherwise = universo

----2

aptoParaPendex :: Universo -> Bool
aptoParaPendex = any ((< 45).edad) 
{-
Utilizando Orden Superior:
aptoParaPendex = any $ (< 45).edad
-}

energiaTotalUniverso :: Universo -> Number
energiaTotalUniverso  = sum . map energia . filter ((>1).length.habilidades) 
--energiaTotalUniverso universo = foldr ((+).energia) 0 (filter ((>1).length.habilidades) universo) 

--3--

type Gema = Personaje -> Personaje

mente :: Number ->  Gema 
mente  = quitarEnergia 

quitarEnergia :: Number -> Gema
quitarEnergia valor personaje = personaje{energia= energia personaje - valor}

alma :: Habilidad -> Gema 
alma habilidad personaje = quitarEnergia 10 personaje{
    habilidades = filter (/= habilidad) $ habilidades personaje
}

espacio :: String -> Gema 
espacio planetaNuevo personaje = quitarEnergia 20 personaje{
    planeta = planetaNuevo
}

poder :: Gema
poder personaje =  personaje{
    energia = 0,
    habilidades = quitarHabilidades $ habilidades personaje
}

quitarHabilidades :: [Habilidad] -> [Habilidad]
quitarHabilidades habilidades | length habilidades <= 2 = []
                              | otherwise = habilidades

tiempo :: Gema
tiempo personaje = (quitarEnergia 50.reducirSegun) personaje

reducirSegun :: Personaje -> Personaje
reducirSegun personaje | edad personaje > 18 = personaje {edad = (max 18. div (edad personaje)) 2}
                       | otherwise =  personaje

gemaLoca :: Gema -> Gema
gemaLoca gema  = gema.gema


--4--

unGuanteleteDeGoma = Guantelete{
    material = "goma",
    gemas = [tiempo,alma "usar Mjolnir",gemaLoca $ alma "programacion loca"]

}


--5--

utilizar :: [Gema] -> Personaje -> Personaje --Gema
utilizar gemas pj = foldr ($) pj gemas
-- utilizar gemas pj = foldl (\pj gema -> gema pj) pj gemas

--6-- 
gemaMasPoderosa :: Personaje -> Guantelete ->  Gema
gemaMasPoderosa pj guante  = gemaDeMayorPoder pj (gemas guante)

gemaDeMayorPoder :: Personaje -> [Gema] ->  Gema
gemaDeMayorPoder _ [gema] = gema
gemaDeMayorPoder pj (gema1:gema2:gemas) | (energia.gema1) pj > (energia.gema2) pj = gemaDeMayorPoder pj (gema2:gemas)
                                        | otherwise = gemaDeMayorPoder pj (gema1:gemas)


--7--
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--1--

type Artefacto = (String,Number)
type Tarea = Heroe -> Heroe

data Heroe = Heroe{
    epiteto :: String,
    reconocimiento :: Number,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
} deriving (Show,Eq)

--2--
between :: Number -> Number -> Number -> Bool
between n1 n2 valor = valor >= n1 && valor <= n2

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria heroe | (> 1000) $ reconocimiento heroe = cambiarEpiteto "El mitico" heroe 
                       | between 500 1000 $ reconocimiento heroe = (cambiarEpiteto "El magnifico".aniadirArtefacto ("Lanza Olimpo",100)) heroe
                       | between 100 500 $ reconocimiento heroe = (cambiarEpiteto "el hoplita".aniadirArtefacto("xiphos",50)) heroe
                       | otherwise = heroe

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto epitetoNuevo heroe = heroe{
    epiteto = epitetoNuevo
}

aniadirArtefacto :: Artefacto -> Heroe -> Heroe
aniadirArtefacto artefacto heroe = heroe{
    artefactos= artefacto : artefactos heroe
}

--3--

encontrarArtefacto :: Artefacto -> Tarea 
encontrarArtefacto (nombre, rareza) = ganarReconocimiento rareza .aniadirArtefacto (nombre, rareza)

ganarReconocimiento valor heroe = heroe{
    reconocimiento = reconocimiento heroe + valor
}

escalarOlimpo :: Tarea 
escalarOlimpo  = aniadirArtefacto (relampago).desecharArtefactos.triplicarRarezaArtefactos.ganarReconocimiento 500

relampago :: Artefacto
relampago = ("relampago",500)

mapRarezaDelArtefacto :: (Number-> Number) -> Artefacto -> Artefacto
mapRarezaDelArtefacto unaFuncion (nombre, rareza) = (nombre, unaFuncion rareza)

mapArtefacto :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefacto unaFuncion heroe = heroe { artefactos = unaFuncion (artefactos heroe) }

triplicarRarezaArtefactos :: Heroe -> Heroe
triplicarRarezaArtefactos = mapArtefacto (map(mapRarezaDelArtefacto (*3)))

desecharArtefactos :: Heroe -> Heroe
desecharArtefactos = mapArtefacto (filter ( (> 1000) . snd)) 

ayudarACruzarLaCalle :: Number -> Tarea
ayudarACruzarLaCalle cuadras = cambiarEpiteto (groso cuadras)

groso :: Number -> String
groso cuadras = "gros" ++ (take cuadras listasDeOs)

listasDeOs = 'o' : listasDeOs
---
data Bestia = Bestia{
    nombre :: String,
    debilidad :: Heroe -> Bool
} deriving (Show,Eq)

matarABestia :: Bestia -> Tarea 
matarABestia bestia heroe | debilidad bestia $ heroe = cambiarEpiteto ("El asesino de" ++ nombre bestia) heroe
                          | otherwise = (pierdePrimerArtefacto.cambiarEpiteto "El cobarde") heroe

pierdePrimerArtefacto = mapArtefacto (drop 1)

--4--

heracles :: Heroe
heracles = Heroe "Guardian del Olimpo" 700 [pistola,relampago] []

pistola = ("pistola",1000)

--5--

matarAlLeonDeNemea ::Tarea
matarAlLeonDeNemea = matarABestia leonDeNemea

leonDeNemea = Bestia "Leon de Nemea" epitetoMayorA20

epitetoMayorA20 :: Heroe -> Bool
epitetoMayorA20 = (>20).length.epiteto

--6--

hacerTarea :: Tarea -> (Heroe -> Heroe)
hacerTarea tarea heroe = tarea heroe{
    tareas = tarea : tareas heroe
}

--7--

presuman :: Heroe -> Heroe -> (Heroe,Heroe)
presuman heroe1 heroe2 = ordenarSegun heroe1 heroe2 

ordenarSegun heroe otroHeroe | reconocimiento heroe > reconocimiento otroHeroe = (heroe,otroHeroe)
                             | reconocimiento heroe < reconocimiento otroHeroe = (otroHeroe,heroe)
                             | sonIgualesReconocimientos heroe otroHeroe && sumatoriaRareza heroe > sumatoriaRareza otroHeroe = (heroe,otroHeroe)
                             | sonIgualesReconocimientos heroe otroHeroe && sumatoriaRareza heroe > sumatoriaRareza otroHeroe = (otroHeroe,heroe)
                             | otherwise = presuman (realizarLaborDelOtro heroe otroHeroe) (realizarLaborDelOtro otroHeroe heroe)

sonIgualesReconocimientos heroe otroHeroe = (== reconocimiento otroHeroe) $ reconocimiento heroe

sumatoriaRareza = sum.map snd.artefactos

realizarLaborDelOtro heroe otroHeroe = realizarLabor otroHeroe. tareas . realizarLabor heroe $ tareas otroHeroe

--9--
type Labor = [Tarea]

realizarLabor :: Heroe -> Labor -> Heroe   
realizarLabor unHeroe labor = foldr ($) unHeroe labor
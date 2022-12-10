module Library where
import PdePreludat
import Data.Char(toUpper)
import Data.Char(isUpper)

doble :: Number -> Number
doble numero = numero + numero

---Punto 1---

type Objeto = Barbaro -> Barbaro

data Barbaro = Barbaro{
    nombre :: String,
    fuerza :: Number,
    habilidades :: [String],
    objetos :: [Objeto]
} deriving (Show,Eq)


---accesors
mapNombre :: (String -> String) -> Barbaro -> Barbaro
mapNombre f unBarbaro = unBarbaro { nombre = f . nombre $ unBarbaro }

mapFuerza :: (Number -> Number) -> Barbaro -> Barbaro
mapFuerza f unBarbaro = unBarbaro { fuerza = f . fuerza $ unBarbaro }

mapHabilidades :: ([String] -> [String]) -> Barbaro -> Barbaro
mapHabilidades f unBarbaro = unBarbaro { habilidades = f . habilidades $ unBarbaro }

mapObjetos :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
mapObjetos f unBarbaro = unBarbaro { objetos = f . objetos $ unBarbaro }

--1--

espada :: Number -> Objeto
espada pesoEspada  = mapFuerza (+ pesoEspada * 2)

agregarHabilidad :: String -> Objeto
agregarHabilidad habilidad barbaro = mapHabilidades (++ [habilidad]) barbaro

amuletoMistico :: String -> Objeto
amuletoMistico = agregarHabilidad

varitaDefectuosa :: Objeto
varitaDefectuosa  =  agregarHabilidad "hacer magia".desaparecerObjetos 

desaparecerObjetos :: Objeto
desaparecerObjetos barbaro = barbaro{objetos=[varitaDefectuosa]}

ardilla :: Objeto 
ardilla = id

cuerda :: Objeto -> Objeto -> Objeto
cuerda ob1 ob2 = ob1.ob2

--2--

megafono :: Objeto
megafono = mapHabilidades (ponerEnMayusculas.concatenar)

concatenar :: [String] -> [String]
concatenar habilidades = [concat habilidades]

ponerEnMayusculas :: [String] -> [String]
ponerEnMayusculas habilidades = map (map toUpper) habilidades

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono

---3---

type Aventura = [Evento]
type Evento = Barbaro -> Bool

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes barbaro = elem "escribir poesia atroz " (habilidades barbaro)

cremalleraDelTiempo :: Evento
cremalleraDelTiempo barbaro = (not.tienePulgares.nombre) barbaro

tienePulgares :: String -> Bool
tienePulgares "faffy" = False
tienePulgares "astro" = False
tienePulgares _ = True


saqueo :: Evento
saqueo barbaro = fuerza barbaro > 80 && elem "robar" (habilidades barbaro)

gritoDeGuerra :: Evento
gritoDeGuerra barbaro = poderGrito barbaro >= (length.concat.habilidades) barbaro

poderGrito :: Barbaro -> Number
poderGrito barbaro = 4 * (length.objetos) barbaro

caligrafia :: Evento 
caligrafia unBarbaro = all tieneMasDe3VocalesYEmpiezaConMayuscula (habilidades unBarbaro)

tieneMasDe3VocalesYEmpiezaConMayuscula :: String -> Bool
tieneMasDe3VocalesYEmpiezaConMayuscula habilidad = tieneMasDe3Vocales habilidad && empiezaConMayuscula habilidad

tieneMasDe3Vocales :: String -> Bool
tieneMasDe3Vocales habilidad = ((>3).length.filter esVocal) habilidad

esVocal :: Char -> Bool
esVocal 'a' = True
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal _ = False

empiezaConMayuscula :: String -> Bool
empiezaConMayuscula = isUpper.head

ritualDeFechorias :: [Evento] -> Evento
ritualDeFechorias eventos barbaro = pasaUnaAventura any barbaro eventos

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes barbaros aventura = filter (\barbaro -> pasaUnaAventura all barbaro aventura) barbaros

pasaUnaAventura criterio barbaro aventura = criterio (\evento -> evento barbaro) aventura

----4----

sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (cabeza : cola) 
  | elem cabeza cola = cola
  | otherwise = (cabeza:cola)

descendiente :: Barbaro -> Barbaro
descendiente = utilizarObjetos.mapNombre (++ "*"). mapHabilidades sinRepetidos

utilizarObjetos :: Barbaro -> Barbaro
utilizarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro)

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = iterate descendiente unBarbaro
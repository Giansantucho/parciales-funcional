module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Personaje = Personaje {
    nombre :: String,
    salud :: Number,
    elementos :: [Elemento],
    anioPresente :: Number
  } deriving (Show, Eq)

data Elemento = Elemento {
  tipo :: String,
  ataque :: (Personaje-> Personaje),
  defensa :: (Personaje-> Personaje)
}

instance Show Elemento where
  show = tipo

instance Eq Elemento where
  (==) elemento1 elemento2 = tipo elemento1 == tipo elemento2

--1--

mandarAlAnio :: Number -> Personaje -> Personaje 
mandarAlAnio anio pj = pj{
    anioPresente = anio
}

meditar :: Personaje -> Personaje 
meditar = modificarSalud (*1.5)

causarDanio :: Number -> Personaje -> Personaje
causarDanio danio  = modificarSalud (max 0.flip (-) danio)

modificarSalud :: (Number -> Number) -> Personaje -> Personaje
modificarSalud f pj = pj{salud= (f.salud) pj}

---2--- 

esMalvado :: Personaje -> Bool
esMalvado = any ((== "Maldad")).map tipo.elementos

danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce pj elemento = salud pj - salud (ataque elemento pj)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos = filter (esEnemigoMortal personaje) enemigos

esEnemigoMortal personaje enemigo =(any (tieneAtaqueMortal personaje) . elementos) enemigo

tieneAtaqueMortal personaje elemento = danioQueProduce personaje elemento == salud personaje

concentracion :: Number -> Elemento
concentracion nivelDeConcentracion =
  Elemento { tipo = "Magia",
               defensa = (!! nivelDeConcentracion) . iterate meditar }

esbirrosMalvados :: Number -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad unEsbirro

unEsbirro :: Elemento
unEsbirro = Elemento "Maldad" (causarDanio 1) id

jack :: Personaje
jack = Personaje {
  nombre = "Jack",
  salud = 300,
  elementos = [concentracion 3, katanaMagica],
  anioPresente = 200
}

katanaMagica = Elemento "Magia" (causarDanio 1000) id

aku anio saludInicial = Personaje {
  nombre = "Aku",
  salud = saludInicial,
  anioPresente = anio,
  elementos = concentracion 4 : portalAlFuturoDesde anio : esbirrosMalvados (100 * anio)
}
portalAlFuturoDesde anio = Elemento "Magia" (mandarAlAnio anioFuturo) (aku anioFuturo.salud)
  where anioFuturo = anio + 2800

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
 |estaMuerto atacante = (defensor, atacante)
 |otherwise = luchar proximoAtacante proximoDefensor
 where proximoAtacante = usarElementos ataque defensor (elementos atacante)
       proximoDefensor = usarElementos defensa atacante (elementos atacante)

usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl afectar personaje (map funcion elementos)

afectar personaje funcion = funcion personaje

estaMuerto = ((==0).salud)
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)


data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)


-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)


data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles

between n m x = elem x [n .. m]



  ----Resolución Parcial----
type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro{
    velocidad = 10, 
    precision = precisionJugador habilidad * 2, 
    altura = 0}


madera :: Palo
madera habilidad = UnTiro{
    velocidad = 100, 
    precision = precisionJugador habilidad / 2, 
    altura = 5}

hierro :: Number -> Palo
hierro n habilidad = UnTiro{
    velocidad = fuerzaJugador habilidad * n, 
    precision = precisionJugador habilidad / n , 
    altura = (n-3) `max` 0}

palos :: [Palo]
palos = [madera, putter] ++ map hierro [1 .. 10]

---2---

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

----3----
tiroDetenido = UnTiro 0 0 0

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal
  | puedeSuperar obstaculo tiroOriginal = efectoLuegoDeSuperar obstaculo tiroOriginal
  | otherwise = tiroDetenido

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita

data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
  }

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = UnTiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}

vaAlRasDelSuelo = (==0).altura

laguna :: Number -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5.altura) tiro
efectoLaguna :: Number -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo}


hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo
superaHoyo tiro = (between 5 20.velocidad) tiro && vaAlRasDelSuelo tiro
efectoHoyo _ = tiroDetenido


--4--

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Number
cuantosObstaculosConsecutivosSupera tiro [] = 0
cuantosObstaculosConsecutivosSupera tiro (obstaculo:obstaculos) | puedeSuperar obstaculo tiro = 1 + cuantosObstaculosConsecutivosSupera (efectoLuegoDeSuperar obstaculo tiro) obstaculos
                                                | otherwise = 0

---4c---
maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord x => (t -> x) -> (t -> t -> t)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos
  = maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos.golpe jugador) palos

--5--

jugadorDeTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo
  = (map (padre.jugadorDeTorneo) . filter (not . gano puntosDeTorneo)) puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador
  = (all ((< puntosGanados puntosDeUnJugador).puntosGanados)
      . filter (/= puntosDeUnJugador)) puntosDeTorneo
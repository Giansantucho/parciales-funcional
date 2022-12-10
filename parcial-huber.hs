module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Cliente = Cliente{
    nombre :: String,
    domicilio :: String
} deriving (Show,Eq)

data Viaje = Viaje{
    fecha :: (Number,Number,Number),
    cliente :: Cliente,
    costo :: Number
} deriving (Show,Eq)

type CondicionViaje = Viaje -> Bool

data Chofer = Chofer{
   nombreChofer :: String,
   kmAuto :: Number,
   viajesQueTomo :: [Viaje],
   condicionViaje :: CondicionViaje
} deriving (Show,Eq)

---2---

tomaCualquierViaje :: CondicionViaje
tomaCualquierViaje _ = True

viajeDeMasDe200 :: CondicionViaje
viajeDeMasDe200  = (>200).costo 

nLetrasCliente :: Number -> CondicionViaje
nLetrasCliente nLetras  = (> nLetras).length.nombre.cliente

queNoVivaEnLugar :: String -> CondicionViaje
queNoVivaEnLugar lugar  = (/= lugar).domicilio.cliente  

--3--

lucas = Cliente "Lucas" "Victoria"

daniel = Chofer "Daniel" 23500 [Viaje (20,4,17) lucas 150] (queNoVivaEnLugar "Olivos")

alejandra = Chofer "Alejandra" 180000 [] tomaCualquierViaje

--4--

puedeTomarViaje :: Viaje -> Chofer ->  Bool
puedeTomarViaje  viaje chofer= (condicionViaje chofer) viaje -- condicionViaje chofer $ viaje 

--5--

liquidacionChofer :: Chofer -> Number
liquidacionChofer = sum.map costo.viajesQueTomo
-- liquidacionChofer chofer = foldr ((+) . costo) 0 (viajes chofer)

--6--

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje  = agregarViaje viaje.choferConMenosViajes.filter (puedeTomarViaje viaje) 

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer 
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes ((elQueMenosViajesHizo chofer1 chofer2):choferes)

elQueMenosViajesHizo :: Chofer -> Chofer -> Chofer
elQueMenosViajesHizo chofer1 chofer2
   | cantidadesViajes chofer1 > cantidadesViajes chofer2 = chofer2
   | otherwise = chofer1

cantidadesViajes = length . viajesQueTomo

agregarViaje :: Viaje -> Chofer -> Chofer
agregarViaje viaje chofer = chofer{
    viajesQueTomo = viaje : viajesQueTomo chofer
}


--7--

{-repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

nito = Chofer "Nito Infy" 70000 viajeInfinito $ clienteNombreLargo 3

viajeInfinito = repetirViaje $ Viaje (11, 3, 2017) lucas 50

-- b 
-- liquidacionChofer nito ... no termina nunca!!
-- c pero 
-- puedeTomarViaje (Viaje (2,5,2017) lucas 50) nito
-- True
-- porque no involucra a la lista de viajes
-}
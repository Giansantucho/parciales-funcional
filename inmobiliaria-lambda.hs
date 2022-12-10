module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--Estructuras


type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
 ambientes :: Number,
 superficie :: Number,
 precio :: Number,
 barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
   mail :: Mail,
   busquedas :: [Busqueda]
}

ordenarSegun :: (a-> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
 (ordenarSegun criterio . filter (not . criterio x)) xs ++
 [x] ++
 (ordenarSegun criterio . filter (criterio x)) xs

between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
 Depto 3 80 7500 "Palermo",
 Depto 1 45 3500 "Villa Urquiza",
 Depto 2 50 5000 "Palermo",
 Depto 1 45 5500 "Recoleta"]

--

---1---

mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor f a b = f a > f b

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor f a b = f a < f b

--2--

ubicadoEn :: [Barrio] -> Depto -> Bool
ubicadoEn barrios depto = elem (barrio depto) barrios

cumpleRango :: Ord a => (Depto -> a) -> a -> a -> Depto -> Bool
cumpleRango f n1 n2  = between n1 n2.f 


--3--

cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto busqueda = all (\req -> req depto) busqueda
--cumpleBusqueda depto = all (\req -> req depto)
--cumpleBusqueda depto = all ($ depto)

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> ([Depto] -> [Depto])
buscar busqueda criterio = ordenarSegun criterio . filter (flip cumpleBusqueda busqueda) 
-- buscar busqueda criterio = ordenarSegun criterio . filter (`cumpleBusqueda` busqueda) 
-- buscar busqueda criterio deptos = (ordenarSegun criterio . filter (flip cumpleBusqueda busqueda)) deptos


mailsDePersonasInteresadas :: Depto -> ([Persona] -> [Mail])
mailsDePersonasInteresadas depto = map mail . filter (any(cumpleBusqueda depto).busquedas) 
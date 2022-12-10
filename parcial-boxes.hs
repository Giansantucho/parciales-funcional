module Library where
import PdePreludat
import Data.Complex (magnitude)

doble :: Number -> Number
doble numero = numero + numero

data Auto = Auto{
    combustible :: Number,
    nivelSeguridad :: Number,
    capacidadMaximaTanque :: Number,
    rendimiento :: Number
} deriving (Show,Eq)

--1--

velocidad :: Auto -> Number
velocidad auto = (100 - combustible auto) * nivelSeguridad auto

--2--

type Mantenimiento = Auto -> Auto

cargarCombustible :: Number -> Mantenimiento
cargarCombustible cantCombustible auto = auto{
    combustible = min (combustible auto + cantCombustible) (capacidadMaximaTanque auto)
}

hacerReparacion :: String -> Mantenimiento
hacerReparacion reparacion = aumentarNivelSeguridad (length reparacion)

aumentarNivelSeguridad :: Number-> Mantenimiento
aumentarNivelSeguridad n auto = auto{
    nivelSeguridad = nivelSeguridad auto + (nivelSeguridad auto *  n / 100 )
}

--3-- 

data Repuesto = Repuesto {
descripcion :: String,
magnitud :: Number
} deriving(Show, Eq)

mantenimientoConRepuestos :: [Repuesto] -> Mantenimiento
mantenimientoConRepuestos repuestos = cargarCombustible (magnitudDelPrimero repuestos).aumentarNivelSeguridad (magnitudDelUltimo repuestos).vaciarTanqueYCambiarCapacidad repuestos

listaDeMagnitudes :: [Repuesto] -> [Number]
listaDeMagnitudes = map magnitud

magnitudDelUltimo :: [Repuesto] -> Number
magnitudDelUltimo repuestos = last $ listaDeMagnitudes repuestos

magnitudDelPrimero :: [Repuesto] -> Number
magnitudDelPrimero repuestos = primerValor $ listaDeMagnitudes repuestos

primerValor :: [Number] -> Number
primerValor [numero] = numero
primerValor (numero:numeros) = numero

vaciarTanqueYCambiarCapacidad :: [Repuesto] -> Mantenimiento
vaciarTanqueYCambiarCapacidad repuestos auto | elem "tanque" $ listaDeDescripciones repuestos = (cambiarCapacidadMaxima (esTanque repuestos).tanqueVacio) auto
                                             | otherwise = auto
listaDeDescripciones :: [Repuesto] -> [String]
listaDeDescripciones = map descripcion

tanqueVacio :: Mantenimiento
tanqueVacio auto = auto{
    combustible = 0
}

cambiarCapacidadMaxima :: Number -> Mantenimiento
cambiarCapacidadMaxima n auto = auto{
   capacidadMaximaTanque = n
}

esTanque :: [Repuesto] -> Number
esTanque = maximum.map magnitud.filter ((== "tanque").descripcion)

--4--
deltaSegun :: (Auto -> Number) -> Auto -> Auto-> Number
deltaSegun caracteristica auto autoDos = caracteristica auto - caracteristica autoDos

diferenciaNivelSeguridad :: Auto -> Mantenimiento -> Number
diferenciaNivelSeguridad auto mantenimiento = deltaSegun nivelSeguridad auto (mantenimiento auto)

diferenciaVelocidad :: Auto -> Mantenimiento-> Number
diferenciaVelocidad auto mantenimiento = deltaSegun velocidad auto (mantenimiento auto)

diferenciaLuegoTareas :: (Auto -> Number) -> Auto -> [Mantenimiento]-> Number
diferenciaLuegoTareas caracteristica auto mantenimientos  = deltaSegun caracteristica auto (aplicarMantenimientos auto mantenimientos)

aplicarMantenimientos :: Auto -> [Mantenimiento] -> Auto
aplicarMantenimientos auto mantenimientos = foldr ($) auto mantenimientos 

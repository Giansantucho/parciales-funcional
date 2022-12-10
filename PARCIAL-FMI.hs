module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

---1---
type Recurso = String

data Pais = Pais{
    ingresoPerCapita :: Number,
    activaPublico :: Number,
    activaPrivado :: Number,
    recursos :: [Recurso],
    deuda :: Number
} deriving(Show,Eq)

namibia :: Pais
namibia = Pais 4140 400000 650000 ["mineria","ecoturismo"] 50

---2---
type Estrategia = Pais -> Pais

prestamoNDolares :: Number -> Estrategia
prestamoNDolares n pais = pais{deuda= deuda pais + n * 1.5}

reducirXPuestos :: Number -> Estrategia 
reducirXPuestos puestos pais = pais{
activaPublico = activaPublico pais - puestos,
ingresoPerCapita = ingresoPerCapita pais * (1 - reducirIngreso puestos) 
}

reducirIngreso :: Number -> Number
reducirIngreso puestos | puestos > 100 = 0.2
                         | otherwise = 0.15

explotarRecurso :: Recurso -> Estrategia
explotarRecurso recurso pais = pais{
    deuda = deuda pais - 2,
    recursos = quitarRecurso recurso (recursos pais)
}

quitarRecurso :: Recurso -> ([Recurso] -> [Recurso])
quitarRecurso recurso = filter (/= recurso) 

blindaje :: Estrategia
blindaje pais = (prestamoNDolares (pbi pais * 0.5).reducirXPuestos 500) pais

pbi :: Pais -> Number
pbi pais = ingresoPerCapita pais * (activaPrivado pais + activaPublico pais)

----3----
type Receta = [Estrategia]

receta :: Receta
receta = [prestamoNDolares 200, explotarRecurso "mineria"]

aplicarReceta ::Receta -> Estrategia
aplicarReceta  receta pais = foldr ($) pais receta


---4---

puedeZafar :: [Pais] -> [Pais]
puedeZafar  = filter $ elem "petroleo".recursos 

totalDeuda :: [Pais] -> Number
totalDeuda = sum.map deuda

---5---

estaOrdenado :: Pais -> [Receta] -> Bool
estaOrdenado pais [receta] = True
estaOrdenado pais (receta1:receta2:recetas)
     = revisarPBI receta1 pais <= revisarPBI receta2 pais && estaOrdenado pais (receta2:recetas)
     where revisarPBI receta = pbi . aplicarReceta receta


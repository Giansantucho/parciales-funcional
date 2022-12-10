module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Idioma = String

data Turista = Turista
  { cansancio :: Number
  , stress    :: Number
  , solitario :: Bool
  , idiomas   :: [Idioma]
  } deriving (Show,Eq)

cambiarStress :: Number -> Turista -> Turista
cambiarStress delta turista = turista {stress = stress turista + delta}

cambiarStressPorcentual :: Number -> Turista -> Turista
cambiarStressPorcentual porciento turista =
  cambiarStress (div (porciento * stress turista) 100) turista

cambiarCansancio :: Number -> Turista -> Turista
cambiarCansancio delta turista = turista {cansancio = cansancio turista + delta}

aprenderIdioma :: Idioma -> Turista -> Turista
aprenderIdioma idioma turista = turista {idiomas = idioma : idiomas turista}

acompaniado :: Turista -> Turista
acompaniado turista = turista {solitario = False}

---1---

ana :: Turista
ana =
  Turista { cansancio = 0 , stress = 20, solitario = False, idiomas = ["espaniol"] }

beto :: Turista
beto =
  Turista { cansancio = 15, stress = 15, solitario = True, idiomas = ["aleman"] }

cathi :: Turista
cathi =
  Turista { cansancio = 15, stress = 15, solitario = True, idiomas = ["aleman", "catalan"] }

type Excursion = Turista -> Turista

playa :: Excursion
playa turista | solitario turista = cambiarCansancio (- 5) turista
              | otherwise = cambiarStress (-1) turista

apreciarPaisaje :: String -> Excursion
apreciarPaisaje paisaje = cambiarStress (- length paisaje)

salirAHablarUnIdioma :: String -> Excursion
salirAHablarUnIdioma idioma  = acompaniado.aprenderIdioma idioma

caminar :: Number -> Excursion
caminar minutos  = cambiarStress (- intensidad minutos).cambiarCansancio (intensidad minutos)

intensidad minutos = minutos / 4

data Marea = Fuerte | Moderada | Tranquila

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte  = cambiarStress 6.cambiarCansancio 10
paseoEnBarco Moderada  = id
paseoEnBarco Tranquila  = salirAHablarUnIdioma "aleman".apreciarPaisaje "mar".caminar 10

hacerExcursion :: Excursion -> (Turista -> Turista)
hacerExcursion excursion = cambiarStressPorcentual (-10).excursion

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista

excursionEducativa ::  Turista -> Excursion-> Bool
excursionEducativa turista = (>0).deltaExcursionSegun (length.idiomas) turista

excursionDesestresante :: Turista -> [Excursion] -> [Excursion]
excursionDesestresante turista = filter (leQuitaStress turista)

leQuitaStress :: Turista -> Excursion -> Bool
leQuitaStress turista = (<= -3).deltaExcursionSegun stress turista

---3---

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarPaisaje "cascada", caminar 40, playa, salirAHablarUnIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina marea = [paseoEnBarco marea, dependeMarea marea, paseoEnBarco marea]

dependeMarea :: Marea -> Excursion
dependeMarea Fuerte = apreciarPaisaje "lago"
dependeMarea _ = playa

----a----

hacerTour :: Turista -> Tour ->  Turista
hacerTour  turista tour = foldr ($) (cambiarStress (length tour)turista) tour

---b---

toursConvincentes :: [Tour] -> Turista -> Bool
toursConvincentes tours turista = any (esConvincente turista) tours

esConvincente :: Turista -> Tour -> Bool
esConvincente turista = any (dejaAcompaniado turista) . excursionDesestresante turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista = not . solitario . flip hacerExcursion turista


efectividad :: Tour -> [Turista] -> Number
efectividad tour = sum . map (espiritualidadAportada tour) . filter (`esConvincente` tour)

espiritualidadAportada :: Tour -> Turista -> Number
espiritualidadAportada tour = negate . deltaRutina tour

deltaRutina :: Tour -> Turista -> Number
deltaRutina tour turista =
  deltaSegun nivelDeRutina (hacerTour turista tour) turista

nivelDeRutina :: Turista -> Number
nivelDeRutina turista = cansancio turista + stress turista


data Elemento = Elemento {
    tipo :: String,
    ataque :: (Personaje -> Personaje),
    defensa :: (Personaje -> Personaje)
}

data Personaje = Personaje {
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
}

--------------
-- Punto 01 --
--------------

type Transformacion = Personaje -> Personaje

mandarAlAnio :: Int -> Transformacion
mandarAlAnio unAnio unPersonaje = unPersonaje { anioPresente = unAnio}

meditar :: Transformacion
meditar unPersonaje = unPersonaje { salud = salud unPersonaje + (salud unPersonaje * 1.5) }

causarDanio :: Float -> Transformacion
causarDanio cantidadSalud unPersonaje = unPersonaje { salud =  max 0 (salud unPersonaje - cantidadSalud) }

--------------
-- Punto 02 --
--------------

esMalvado :: Personaje -> Bool
esMalvado unPersonaje = any (esDeTipo "Maldad") . elementos $ unPersonaje

esDeTipo :: String -> Elemento -> Bool
esDeTipo unTipo unElemento = tipo unElemento == unTipo

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce unPersonaje unElemento = salud unPersonaje - salud (ataque unElemento unPersonaje) 

type Enemigo = Personaje

enemigosMortales :: Personaje -> [Enemigo] -> [Enemigo]
enemigosMortales unPersonaje enemigos = filter (esEnemigoMortal unPersonaje) enemigos

esEnemigoMortal :: Personaje -> Enemigo -> Bool
esEnemigoMortal unPersonaje unEnemigo = any (tieneAtaqueMortal unPersonaje) . elementos $ unEnemigo

tieneAtaqueMortal :: Personaje -> Elemento -> Bool
tieneAtaqueMortal unPersonaje unElemento = estaMuerto . ataque unElemento $ unPersonaje

estaMuerto :: Personaje -> Bool
estaMuerto = (== 0) . salud 

--------------
-- Punto 03 --
--------------

concentracion :: Int -> Elemento
concentracion nivelConcentracion = Elemento { 
    tipo = "Magia", 
    ataque = noHacerNada, 
    defensa = (!! nivelConcentracion) . iterate meditar
    -- equivalente con composición y aplicación parcial para:
    -- defensa = (\personaje -> iterate meditar personaje !! nivelDeConcentracion) }
    -- otra versión super interesante:
    -- defensa = foldr1 (.) (replicate nivelDeConcentracion meditar)
    -- por ejemplo (concentracion 3) resultaría en meditar.meditar.meditar
} 

noHacerNada = id

esBirrosMalvados :: Int -> [Elemento]
esBirrosMalvados unaCantidad = replicate unaCantidad birrosMalvados

birrosMalvados :: Elemento
birrosMalvados = Elemento "Maldad" (causarDanio 1) noHacerNada

jack :: Personaje
jack = Personaje "Jack" 300 [concentracion 3, katanaMagica] 200

katanaMagica :: Elemento
katanaMagica = Elemento "Magia" (causarDanio 1000) noHacerNada

aku :: Int -> Float -> Personaje
aku unAnio cantidadSalud = Personaje {
    nombre = "Aku",
    salud = cantidadSalud,
    elementos = [concentracion 4, esBirrosMalvados (100 * unAnio), portalAlFuturo unAnio],
    anioPresente = unAnio
}

portalAlFuturo :: Int -> Elemento
portalAlFuturo unAnio = Elemento "Magia" (mandarAlAnio anioFuturo) (aku anioFuturo . salud) 
    where anioFuturo unAnio + 2800

--------------
-- Punto 04 --
--------------

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor 
    | estaMuerto atacante = (defensor, atacante)
    | otherwise           = luchar proximoAtacante proximoDefensor
    where proximoAtacante = usarElementos ataque defensor (elementos atacante)
          proximoDefensor = usarElementos defensa atacante (elementos atacante)

-- Abstraemos cómo hacer para usar uno de los efectos de un conjunto de elementos sobre un personaje
usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl afectar personaje (map funcion elementos)

afectar personaje funcion = funcion personaje
afectar' = flip ($)

--------------
-- Punto 05 --
--------------

-- f :: (Eq t1, Num t2) => (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]

-- f x y z
--     | y 0 == z = map (fst . x z)
--     | otherwise = map (snd . x (y 0))
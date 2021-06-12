module Library where
import PdePreludat


{-
*Punto 1
-}

data Persona = Persona {
    nombre:: String,
    satisfaccion:: Number,
    emocion:: Number,
    cultura:: Number
} deriving Show

juan = Persona "Juan" 20 30 40
ana = Persona "Ana" 10 20 60
grinch = Persona "Grinch" 1 150 1
leopoldo = Persona "Leopoldo" 1 1 1

{-
*Punto 2 Atracciones
-}

type Atraccion = Persona -> Persona

aumentarEmocion cantidad persona = persona {emocion = (emocion persona) + cantidad}

aumentarSatisfaccion cantidad persona = persona {satisfaccion = (satisfaccion persona) + cantidad}

disminuirEmocion cantidad persona = aumentarEmocion ((-1)*cantidad) persona

disminuirSatisfaccion cantidad persona = aumentarSatisfaccion((-1)*cantidad) persona

aumentarCultura cantidad persona = persona {cultura = (cultura persona) + cantidad}

--Montaña rusa

montañaRusa :: Number -> Number -> Atraccion
montañaRusa velocidad altura persona 
    | (velocidad > 50) = aumentarEmocion ((+altura).(*0.15) $ velocidad) persona
    | otherwise = disminuirEmocion ((*0.05).emocion $ persona) . disminuirSatisfaccion ((*0.1).satisfaccion $ persona) $ persona

--Caida libre

caidaLibre :: Number -> Atraccion
caidaLibre altura persona = aumentarEmocion ((*0.2) altura) persona

--Mundo maya

mundoMaya :: Atraccion
mundoMaya persona = aumentarEmocion ((*0.1).emocion $ persona) . aumentarCultura ((*0.2).cultura $ persona) $ persona

--Show de magia

showDeMagia :: Atraccion
showDeMagia persona 
    | ((>50).cultura $ persona) = aumentarSatisfaccion (20) persona
    | otherwise = aumentarEmocion (30) persona

{-
* Punto 3
-}

visitar :: [Atraccion] -> Persona -> Persona
visitar atracciones persona = foldl (flip($)) persona atracciones

{-
* Punto 4
-}

--Mi funcion inventada te deja como, asistieses a un showDeMagia y luego a una montaña rusa de 100km/h y 50mts

atraccionInventada = (\persona -> (montañaRusa 100 50).showDeMagia $ persona)
{-
*Library> visitar [montañaRusa 20 20,caidaLibre 50, mundoMaya, showDeMagia,(\persona -> (montañaRusa 100 50).showDeMagia $ persona)] ana
Persona {nombre = "Ana", satisfaccion = 29, emocion = 100.09, cultura = 86.4}
-}
todas = [montañaRusa 20 20,caidaLibre 50, mundoMaya, showDeMagia,(\persona -> (montañaRusa 100 50).showDeMagia $ persona)]
{-
visitar todas ana
Persona {nombre = "Ana", satisfaccion = 29, emocion = 100.09, cultura = 86.4}
-}

{-
* Punto 5
-}

estaSatisfecha = (>50).satisfaccion
estaEmocionada = (>60).emocion

estanFelices :: [Persona] -> Bool
estanFelices personas = all (estaSatisfecha).filter (estaEmocionada.mundoMaya.montañaRusa 80 10) $ personas

{-
* Punto 6
-}
cumpleContenta persona = (>100).(+ emocion persona).satisfaccion $ persona

estaContenta :: [Atraccion] -> Persona -> Bool
estaContenta atracciones persona = cumpleContenta.visitar atracciones $ persona

{-
* Punto 7
-}
--a No se puede, nunca terminaría ya que requiere procesar toda la lista para aplicar el fold.
atraccionesInfinitas = mundoMaya : atraccionesInfinitas
--b
h f xs = (head.filter f) xs

personasInfinitas = ana: personasInfinitas
--Se puede gracias a la evaluación diferida de Haskell, pero depende de si en la lista infinita de personas hay una que cumpla la condición de estar contenta.

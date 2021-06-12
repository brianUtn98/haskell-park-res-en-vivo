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



module Lib () where
import Data.List(find, delete)
import GHC.IO.Handle.Types (HandleType(ClosedHandle))

data Serie 
    = Serie {
    nombre::String,
    reparto:: [Actor],
    presupuesto::Int,
    temporadas:: Int,
    rating:: Int,
    estado:: Estados,
    bienestar:: Int
} deriving (Show,Eq)

data Actor 
    = Actor {
    nombreAct:: String,
    sueldo:: Int,
    restricciones:: [String]

}deriving (Show,Eq)




unaSerie = Serie "Succesion" actor1 5000 3 2 Cancelado 3
unaSerie2 = Serie "MrRobot" actor1 3000 3 2 EnEmision 3

data Estados = Cancelado | EnEmision deriving(Show,Eq)

---------------
--- Punto 1 ---
---------------
serieEnRojo:: Int -> Serie -> Bool
serieEnRojo presupuesto sueldoPedido = presupuesto > sumarPresupuesto sueldoPedido 

actor1 = [ Actor "Nat" 1000 ["Desc"] , Actor "Milo" 1000 ["Desc","A","B"],Actor "3AM" 1000 ["Desc","A","B"], Actor "MORAT" 1000 ["Desc","A","B"]]

john :: Actor
john = Actor "John" 1000 ["Desc"]

getSalario :: Actor -> Int
getSalario (Actor _ salario _) = salario

sumarPresupuesto:: Serie -> Int
sumarPresupuesto serie =  sum (map (getSalario) (reparto serie))

getRest :: Actor -> Int
getRest (Actor _ _ restricciones) = length restricciones

cantidadDeRestricciones::[Actor]-> Int 
cantidadDeRestricciones actores = sum (map (getRest) actores)

esProblematica:: Serie -> Bool
esProblematica serie = (3 < cantidadDeRestricciones (reparto serie)) && sonMasDeTres (reparto serie)

sonMasDeTres:: [Actor] -> Bool
sonMasDeTres  = (3<).length.filter (1<).map (getRest)


johnydeep::Actor
johnydeep = Actor "Jhony Deep" 20000000 []

helena::Actor
helena = Actor "Helena Bonham" 15000000 []

favoritismos:: Serie ->[Actor] -> Serie
favoritismos serie  = actoresFav (borrarAct serie)

borrarAct:: Serie -> Serie
borrarAct serie = serie {reparto = drop 2 (reparto serie)}

actoresFav:: Serie -> [Actor] -> Serie
actoresFav serie actores = serie { reparto = actores ++ reparto serie}


type Productor = Serie -> Serie

timBurton :: Productor
timBurton serie = favoritismos serie [johnydeep, helena]

gatopardeitor:: Productor
gatopardeitor serie = serie

estireitor:: Productor
estireitor serie = serie { temporadas =  temporadas serie * 2 }

desespereitor:: Productor
desespereitor = gatopardeitor.timBurton 


canceleitor:: Int -> Int -> Serie -> Serie
canceleitor presupuesto numero serie
    |serieEnRojo presupuesto serie || rating serie < numero  = serie{ estado =  Cancelado }
    |otherwise  = serie{ estado =  EnEmision }


{-. Calcular el bienestar de una serie, en base a la sumatoria de estos conceptos:
- Si la serie tiene estimadas más de 4 temporadas, su bienestar es 5, de lo contrario
es (10 - cantidad de temporadas estimadas) * 2
- Si la serie tiene menos de 10 actores, su bienestar es 3, de lo contrario es (10 -
cantidad de actores que tienen restricciones), con un mínimo de 2
Aparte de lo mencionado arriba, si la serie está cancelada, su bienestar es 0 más
allá de cómo diesen el bienestar por longitud y por reparto.
-}

calcularTemp:: Serie -> Serie
calcularTemp serie
    | temporadas serie > 4 = serie {bienestar = bienestar serie + 5}
    | otherwise            = serie {bienestar = bienestar serie +  (10 - temporadas serie)*2} 

calcularLargo:: Serie -> Serie
calcularLargo serie
    | length (getAct serie) < 10 = serie {bienestar =  bienestar serie + 3}
    | otherwise                  = serie {bienestar = bienestar serie +  10 - cantidadConResticcion serie}

calcularEstado:: Serie -> Serie
calcularEstado serie
    | estado serie == Cancelado = serie {bienestar = bienestar serie +  0}
    | otherwise                 = undefined


calcularBienestar::Serie -> Serie
calcularBienestar = calcularLargo.calcularEstado.calcularTemp


getAct :: Serie -> [Actor]
getAct (Serie _ reparto _ _ _ _ _) = reparto

getRestriccion :: Actor -> [String]
getRestriccion (Actor _ _ restricciones) = restricciones

cantidadConResticcion:: Serie -> Int
cantidadConResticcion serie = length.filter (>2).map (length.getRestriccion) $ reparto serie



{-
Dada una lista de series y una lista de productores, aplicar para cada serie el
productor que la haga más efectiva: es decir, el que le deja más bienestar.

-}



efectiva:: [Serie] -> [Productor] -> [Serie]
efectiva serie productor = map (flip masEfectiva productor ) serie

masEfectiva:: Serie -> [Productor]-> Serie
masEfectiva serie productor = foldl (flip ($)) serie productor

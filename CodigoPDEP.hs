
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
    estado:: Estados
} deriving (Show,Eq)

data Actor 
    = Actor {
    nombreAct:: String,
    sueldo:: Int,
    restricciones:: [String]

}deriving (Show,Eq)




unaSerie = Serie "Succesion" actor1 5000 3 2 Cancelado

data Estados = Cancelado | EnEmision deriving(Show,Eq)

{-. Saber si la serie está en rojo, esto es si el presupuesto no alcanza a cubrir lo
que quieren cobrar todos los actores.
b. Saber si una serie es problemática, esto ocurre si tienen más de 3 actores o
actrices con más de 1 restricción-}

---------------
--- Punto 1 ---
---------------
serieEnRojo:: Int -> Serie -> Bool
serieEnRojo presupuesto sueldoPedido = presupuesto > sumarPresupuesto sueldoPedido 

actor1 = [ Actor "Nat" 1000 ["Desc","A","B"], Actor "Milo" 1000 ["Desc","A","B"],Actor "3AM" 1000 ["Desc","A","B"], Actor "MORAT" 1000 ["Desc","A","B"]]

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

esProblematica:: [Actor] -> Bool
esProblematica actores = (3 < cantidadDeRestricciones actores) && sonMasDeTres actores

sonMasDeTres:: [Actor] -> Bool
sonMasDeTres  = (3<).length.filter (1<).map (getRest)


johnydeep::Actor
johnydeep = Actor "Jhony Deep" 20000000 []

helena::Actor
helena = Actor "Helena Bonham" 15000000 []

favoritismos:: Serie ->[Actor] -> Serie
favoritismos serie actores = actoresFav (borrarAct serie) actores

borrarAct:: Serie -> Serie
borrarAct serie = serie {reparto = drop 2 (reparto serie)}

actoresFav:: Serie -> [Actor] -> Serie
actoresFav serie actores = serie { reparto = actores ++ reparto serie}

timBurton :: Serie -> Serie
timBurton serie = favoritismos serie [johnydeep, helena]

gatopardeitor:: Serie -> Serie
gatopardeitor serie = serie

estireitor::Serie -> Serie
estireitor serie = serie { temporadas =  (temporadas serie)*2 }

desespereitor::Serie -> Serie
desespereitor = gatopardeitor.timBurton 

canceleitor:: Int-> Int -> Serie -> Serie
canceleitor presupuesto numero serie
    |(serieEnRojo presupuesto serie)||(rating serie) < numero  = serie{ estado =  Cancelado }
    |otherwise  = serie{ estado =  EnEmision }




module Lib () where
import Data.List(find, delete)
import GHC.IO.Handle.Types (HandleType(ClosedHandle))

data Auto = Auto {

    color:: Color,
    velocidad::Int,
    distancia:: Int
    }deriving (Show,Eq)


data Carrera = Carrera {
    estado:: String

}deriving (Show, Eq)


data Color = Azul|Rojo|Blanco|Negro deriving (Show, Eq)

auto1 = Auto Azul 100 25
auto2 = Auto Rojo 110 16
auto3 = Auto Blanco 110 5
auto4 = Auto Negro 110 5

lista = [auto2, auto3, auto4]

{-
Declarar los tipos Auto y Carrera como consideres convenientes para representar la información indicada
 y definir funciones para resolver los siguientes problemas:

Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay entre
ellos (en valor absoluto) es menor a 10.

Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va 
ganando a todos (por haber recorrido más distancia que los otros).

Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que 
le van ganando.

-}

estaCerca:: Auto -> [Auto] -> Bool
estaCerca auto  = any (10>).map (abs.(distancia auto -)).map (getDistancia)

vaTranquilo:: Auto -> [Auto] -> Bool
vaTranquilo auto autos = vaGanando auto autos && not (estaCerca auto autos)

getDistancia:: Auto -> Int
getDistancia  (Auto _ _ distancia) = distancia

vaGanando:: Auto -> [Auto]-> Bool
vaGanando   miAuto  = all (distancia miAuto >) . map (getDistancia)

puesto:: Auto -> [Auto] -> Int
puesto auto = (+1).length.filter(distancia auto <).map (getDistancia)

corra:: Auto ->  Int -> Auto
corra auto  tiempo = auto {distancia = distancia auto + velocidad auto*tiempo}

alterarVelocidad:: Auto -> Int -> Int
alterarVelocidad   auto = (+)(velocidad auto)

bajarVelocidad:: Auto -> Int -> Auto
bajarVelocidad auto speed
    |velocidad auto > 0 = auto {velocidad = velocidad auto - alterarVelocidad auto speed}
    |otherwise = auto

terremoto:: [Auto] -> Auto -> [Auto]
terremoto  autos auto 
    |estaCerca  auto autos = map bajarCincuenta autos
    |otherwise             = autos

bajarCincuenta:: Auto-> Auto
bajarCincuenta auto = auto { velocidad = velocidad auto - 50 }

miguelitos:: [Auto] -> Int  -> [Auto]
miguelitos   autos bajar = map (flip siVoyGanando bajar) autos

siVoyGanando:: Auto -> Int -> Auto
siVoyGanando   auto bajar = auto {velocidad = velocidad auto - bajar}

jetPack:: Auto -> Int -> Auto
jetPack auto tiempo = auto {distancia = velocidad auto * 2*tiempo}

simularCarrera :: [Auto] -> [Auto -> Auto] -> [(Int, Color)]
simularCarrera auto autos = [()]


correntTodos:: [Auto] -> Int -> [Auto]
correntTodos autos tiempo= map (flip corra tiempo) autos


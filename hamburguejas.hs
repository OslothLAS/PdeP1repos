
data Hamburguesa = Hamburguesa {
nombreHamburguesa :: String, 
ingredientes :: [Ingrediente]} deriving(Show, Eq)



data Bebida = Bebida {
    nombreBebida :: String, 
    tamanioBebida :: Int, --Cambie Number por int para que compile
    light :: Bool
    } deriving(Show, Eq)

type Ingrediente = String
type Acompaniamiento = String
type Combo = (Hamburguesa, Bebida, Acompaniamiento)
type Condicion = Ingrediente -> Bool
type Alteraciones = Combo -> Combo



hamburguesaSola (h,_,_) = h
bebidaSola (_,b,_) = b
acompaniamientoSolo (_,_,a) = a

informacionNutricional = [("Carne", 250), ("Queso", 50), ("Pan", 20), ("Panceta", 541), ("Lechuga", 5), ("Tomate", 6)]

condimentos = ["Barbacoa","Mostaza","Mayonesa","Salsa big mac","Ketchup"]

comboQyB = (qyb, cocaCola, "Papas")
cocaCola = Bebida "Coca Cola" 2 False
qyb = Hamburguesa "QyB" ["Pan", "Carne", "Queso", "Panceta", "Mayonesa", "Ketchup", "Pan"]

--Punto 1

cuantasCalorias :: Ingrediente -> Int
cuantasCalorias ingtrediente
    |esCondimento ingtrediente = 10
    |otherwise = caloriaDeIngrediente ingtrediente

esCondimento :: Ingrediente -> Bool
esCondimento  = flip elem condimentos

caloriaDeIngrediente :: Ingrediente -> Int
caloriaDeIngrediente _ = 5  --No se me ocurre como encontrar las calorias, asique dejo una constante, pero continuo con el parcial

--Una solucion puede ser, pero no compila bien 
--caloriaDeIngrediente :: Ingrediente -> Int
--caloriaDeIngrediente ingrediente = lookup ingrediente informacionNutricional


--Punto 2

esMortal :: Combo -> Bool
esMortal (hamburguesa, bebida, acompaniamiento) = light bebida && not (esEnsalada acompaniamiento) || esUnaBomba hamburguesa

esEnsalada:: Acompaniamiento -> Bool
esEnsalada  = (==) "Ensalada"  

esUnaBomba :: Hamburguesa -> Bool
esUnaBomba  = (>1000).caloriaTotal 

tieneCaloriasMayorQue:: Int -> Hamburguesa -> Bool
tieneCaloriasMayorQue valor hamburguesa = any (masCaloricoQue valor) (ingredientes hamburguesa)

caloriaTotal:: Hamburguesa -> Int
caloriaTotal hamburguesa =  sum (map caloriaDeIngrediente (ingredientes hamburguesa))


--Punto 3

agrandarBebida:: Alteraciones
agrandarBebida (hamburguesa, bebida, acompaniamiento)= (hamburguesa, agrandarBebidaSola bebida, acompaniamiento) --Ver despues

agrandarBebidaSola:: Bebida -> Bebida
agrandarBebidaSola bebida = bebida {tamanioBebida = tamanioBebida bebida + 1}

cambiarAcompaniamientoPor:: Acompaniamiento -> Alteraciones
cambiarAcompaniamientoPor acompaniamientoPedido (hamburguesa, bebida,_) = (hamburguesa, bebida, acompaniamientoPedido)

peroSin:: Condicion -> Alteraciones
peroSin condicion (hamburguesa,bebida,acomp) = (removerIngrediente condicion hamburguesa ,bebida,acomp) 

removerIngrediente::  Condicion -> Hamburguesa -> Hamburguesa
removerIngrediente  condicion hamburguesa = hamburguesa {ingredientes = filter condicion (ingredientes hamburguesa) }

masCaloricoQue:: Int -> Ingrediente -> Bool
masCaloricoQue valor = (>valor).caloriaDeIngrediente

queso :: Condicion
queso = (==) "Queso"



--Punto 4
--filter not.esMortal (foldl (\acc x -> x acc) comboQyB [agrandarBebida, cambiarAcompaniamientoPor "Ensalada", peroSin esCondimento , peroSin masCaloricoQue 400, peroSin queso])

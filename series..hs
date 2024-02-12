
data Actor = UnActor {
    nombre :: String,
    sueldo :: Int,
    restricciones :: [String]
}

data Serie = UnaSerie{
    nombreSerie :: String,
    actores :: [Actor],
    presupuesto :: Int,
    temporadas :: Int,
    rating :: Int,
    estado :: Estado
}


data Estado = Cancelada | Emision deriving(Eq, Show)


suToque = [timBurton, gatoPardeitor, estireitor, canceleitor 4]
jhonyDeep = UnActor  "Jhony Deep" 20000000 []

bonhamCaerter = UnActor  "Bonham Carter" 150000000 []

estaEnRojo :: Serie -> Bool
estaEnRojo serie = sueldoTotalActores serie < presupuesto serie

sueldoTotalActores :: Serie -> Int
sueldoTotalActores serie = sum (map sueldo (actores serie))

esProblematica :: Serie -> Bool
esProblematica = (>3).tieneMasDeUnaRestriccion

tieneMasDeUnaRestriccion:: Serie -> Int
tieneMasDeUnaRestriccion serie = length (filter masDeUnaRestriccion (actores serie)) 

masDeUnaRestriccion:: Actor -> Bool
masDeUnaRestriccion actor = length (restricciones actor) > 1


conFavoritismos:: [Actor] -> Serie -> Serie
conFavoritismos actoresNuevos serie = serie {actores = reemplazarActores actoresNuevos serie}

reemplazarActores :: [Actor] -> Serie -> [Actor]
reemplazarActores actoresNuevos serie = drop 2 (actores serie) ++ actoresNuevos 


timBurton:: Serie -> Serie
timBurton = conFavoritismos [jhonyDeep, bonhamCaerter] 

gatoPardeitor :: Serie -> Serie
gatoPardeitor = undefined

estireitor:: Serie -> Serie
estireitor serie = serie {temporadas = temporadas serie * 2}

canceleitor:: Int -> Serie -> Serie
canceleitor cifra serie 
    | estaEnRojo serie || (rating serie < cifra) = serie {estado = Cancelada}
    | otherwise                                  = serie {estado = Emision}


bienestar :: Serie -> Int
bienestar serie = bienestarPorLongitud serie + bienestarPorReparto serie
    

bienestarPorLongitud:: Serie -> Int
bienestarPorLongitud serie
    | estado serie == Cancelada = 0
    | temporadas serie > 4 = 5
    | otherwise = (10 - temporadas serie) * 2

bienestarPorReparto::Serie -> Int
bienestarPorReparto serie 
    | estado serie == Cancelada = 0
    | length (actores serie) < 10 = 3
    | otherwise = 10 - actoresConRestriccion serie


actoresConRestriccion:: Serie -> Int
actoresConRestriccion serie = length (filter (/=[]) (map restricciones (actores serie)))


sonBeneficiosos:: Serie -> [Serie -> Serie] -> Bool
sonBeneficiosos serie productores = bienestar (sonBeneficiososRecursivo serie productores) > 4

sonBeneficiososRecursivo:: Serie -> [Serie -> Serie] -> Serie
sonBeneficiososRecursivo serie (productor : productores) = foldl (\serie productor -> productor serie) serie productores

esControvertida:: Serie -> Bool
esControvertida  = compararSiguientes.listaDeSueldos


listaDeSueldos :: Serie -> [Int]
listaDeSueldos serie = map sueldo (actores serie)


compararSiguientes :: [Int] -> Bool
compararSiguientes [] = True  -- Si la lista está vacía, todos los elementos siguientes son mayores.
compararSiguientes [_] = True  -- Si la lista tiene solo un elemento, todos los elementos siguientes son mayores.
compararSiguientes (x:y:resto)
    | y > x     = compararSiguientes (y:resto)  -- Si el siguiente elemento es mayor, seguimos evaluando el resto de la lista.
    | otherwise = False

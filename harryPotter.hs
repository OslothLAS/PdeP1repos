data Mago = Mago{
    nombre :: String,
    edad :: Int,
    salud :: Int,
    hechizos :: [Hechizo]
}

type Hechizo = Mago -> Mago

--Punto 1
sectumSempra :: Hechizo
sectumSempra mago
    | salud mago > 10 = quitarVida 10 mago
    | otherwise = quitarVida (salud mago `div`  2) mago


lagrimaFenix :: Int ->  Hechizo
lagrimaFenix  = quitarVida

quitarVida:: Int -> Hechizo
quitarVida valor mago = mago{salud = salud mago - valor} 

obliviate :: Int -> Hechizo
obliviate cantHechizos mago = mago {hechizos = drop cantHechizos (hechizos mago)}

confundus :: Hechizo
confundus mago = primerHechizo mago mago

primerHechizo :: Mago -> Hechizo
primerHechizo = head.hechizos 

--Punto 2

poder :: Mago -> Int
poder mago = salud mago + edad mago * cantidadHechizos mago

cantidadHechizos :: Mago -> Int
cantidadHechizos = length.hechizos

daño :: Mago -> Hechizo -> Int
daño mago hechizo = salud (hechizo mago) 

diferenciaDePoder :: Mago -> Mago -> Int
diferenciaDePoder mago1 mago2 = abs (poder mago1 - poder mago2)

-- Punto 3

data Academia = Academia {
 magos :: [Mago],
 examenDeIngreso :: Mago -> Bool
}


ricenSinHechizos:: Academia -> Bool
ricenSinHechizos academia = any seLlamaRicenYNoTieneHechizos (magos academia) 

--Funciones auxiliares
seLlamaRicenYNoTieneHechizos :: Mago -> Bool
seLlamaRicenYNoTieneHechizos mago = noTieneHechizos mago && seLlamaRicen mago

noTieneHechizos:: Mago -> Bool
noTieneHechizos mago = cantidadHechizos mago == 0

seLlamaRicen:: Mago -> Bool
seLlamaRicen  mago =  nombre mago == "Rincenwind"

alumnos :: Academia -> [String]
alumnos academia = map nombre (magos academia)
--Fin de funciones auxiliares


sonNerds :: Academia -> Bool
sonNerds academia = all tienenMasHechizos (magosViejos academia)

magosViejos:: Academia -> [Mago]
magosViejos academia =  filter ((>50).edad) (magos academia) 

tienenMasHechizos:: Mago -> Bool
tienenMasHechizos mago = cantidadHechizos mago > 3*edad mago

noPasanSiRinden:: Academia -> Int
noPasanSiRinden academia = length (filter (pasaExamen academia) (magos academia))

pasaExamen :: Academia -> Mago -> Bool
pasaExamen    = examenDeIngreso 

--Punto 4

funcion parametro [funciones] = funciones
funcion parametro (primerElemento: segundoElemento: cola)
     | parametro primerElemento >= parametro segundoElemento = funcion parametro (primerElemento:cola)
     | otherwise = funcion parametro (segundoElemento : cola)

--En el primer caso cuando hay un solo elemento, devuelve ese elemmento
--si hay mas de una funcion toma una lista de funciones que devuelven un valor ordenable y un parametro, 
--compara este parametro con la primer y segunda funcion y segun el valor que arroja
--acomoda la funcion en la cabeza o en el segundo lugar
--basicamente ordena las funciones dependiendo de los resultados que arroja un
--parametro X a cada funcion de manera recursiva

mejorHechizoContra :: Mago -> Mago -> Hechizo
mejorHechizoContra magoPrimero magoSegundo = funcion (saludDespuesDelHechizo magoPrimero) (hechizos magoSegundo)

mejorOponente :: Mago -> Academia -> Mago
mejorOponente mago academia = funcion (diferenciaDePoder mago) (magos academia)

saludDespuesDelHechizo :: Mago-> Hechizo  -> Int
saludDespuesDelHechizo  mago hechizo = salud (hechizo mago)


--Punto 5
noPuedeGanarle :: Mago -> Mago -> Bool
noPuedeGanarle mago1 mago2 = salud mago1 == leTiraTodosLosHechizos mago1 mago2

leTiraTodosLosHechizos:: Mago -> Mago -> Int
leTiraTodosLosHechizos magoPrimero magoSegundo = salud (foldl (\acc x -> x acc) magoPrimero (hechizos magoSegundo))


foldl (\acc x -> x acc) elemento (listaDeFunciones)

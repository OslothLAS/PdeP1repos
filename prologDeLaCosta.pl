puestoDeComida(hamburguesa, 2000).
puestoDeComida(panchitoConPapas, 1500).
puestoDeComida(lomitoCompleto, 2500).
puestoDeComida(caramelos, 0).

atraccion(autitosChocadores, tranquila(chicosYAdultos)).
atraccion(casaEmbrujada, tranquila(chicosYAdultos)).
atraccion(laberinto, tranquila(chicosYAdultos)).
atraccion(tobogan, tranquila(chicos)).
atraccion(calesita, tranquila(chicos)).

atraccion(barcoPirata, intensa(14)).
atraccion(tazasChinas, intensa(6)).
atraccion(simulador3D, intensa(2)).

atraccion(abismoMortalRecargada, montaniaRusa(3, 134)).
atraccion(paseoPorElBosque, montaniaRusa(0, 45)).

atraccion(esperoQueHayasTraidoUnaMudaDeRopa, acuatica()).
atraccion(elTorpedoSalpicon, acuatica()).

visitante(eusebio, 80, 3000, viejitos).
sentimiento(eusebio, 50, 0).
visitante(carmela, 80, 0, viejitos).
sentimiento(carmela, 0, 25).


bienestar(Visitante, Estado):-
    sentimiento(Visitante, Hambre, Aburrimiento),
    Suma is Hambre + Aburrimiento,
    estadoDeBienestar(Suma, Visitante, Estado).

estadoDeBienestar(0, Visitante, felicidadPlena).
    estaAcompaniado(Visitante).

estadoDeBienestar(0, Visitante, podriaEstarMejor):-
    not(estaAcompaniado(Visitante)).

estadoDeBienestar(Suma, _, podriaEstarMejor):-
    between(1, 50, Suma).
        
estadoDeBienestar(Suma,_,necesitaEntretenerse):-
     between(51, 99, Suma).

estadoDeBienestar(Suma, _ , seQuiereIrACasa).
    Suma >= 100.


estaAcompaniado(Visitante):-
    integranteDeGrupo(UnVisitante, Grupo),
    integranteDeGrupo(Visitante, Grupo),
    UnVisitante \= Visitante.

integranteDeGrupo(Visitante, Grupo) :-
  visitante(Visitante, _, _, Grupo).

Saber si un grupo familiar puede satisfacer su hambre con cierta comida. Para que esto ocurra,
cada integrante del grupo debe tener dinero suficiente como para comprarse esa comida y esa 
comida, a la vez, debe poder quitarle el hambre a cada persona. La hamburguesa satisface a
quienes tienen menos de 50 de hambre; el panchito con papas sólo le quita el hambre a los 
chicos; y el lomito completo llena siempre a todo el mundo. Los caramelos son un caso particular: 
sólo satisfacen a las personas que no tienen dinero suficiente para pagar ninguna otra comida.


puedeaSatisfacer(Grupo, Comida):-
    grupo(Grupo),
    puestoDeComida(Comida,_),
    forall(visitante(Visitante,_,_,Grupo), puedeComprarySatisfacer(Visitante, Comida)).

grupo(Grupo) :-
  visitante(_, _, _, Grupo).

puedeComprarySatisfacer(Visitante, Comida):-
    puedeSatisfacerComida(Visitante, Comida),
    puedeComprar(Visitante, Comida).

puedeComprar(Visitante, Comida):-
    visitante(Visitante, _,Dinero,_),
    puestoDeComida(Comida, Precio),
    Precio =< Dinero.

puedeSatisfacerComida(Visitante, hamburguesa):-
    sentimiento(Visitante, Hambre,_),
    Hambre < 50.

puedeSatisfacerComida(Visitante, panchitoConPapas):-
    esChico(Visitante).

puedeSatisfacerComida(_, lomitoCompleto).

puedeaSatisfacer(Visitante, caramelos):-
    not(puedePagarAlgo(Visitante)).

puedePagarAlgo(Visitante):-
    puedeComprar(Visitante, Comida),
    Comida \= caramelos.


lluviaDeHamburguesas(Visitante, Atraccion):-
    puedeComprar(Visitante, hamburguesa),
    efectoDeActraccion(Visitante, Atraccion).

efectoDeActraccion(_,Atraccion):-
    esIntensa(Atraccion).

efectoDeActraccion(_,Atraccion):-
    esMontaniaPeligrosa(Atraccion).

efectoDeActraccion(_,tobogan).

esIntensa(Atraccion):-
    atraccion(Atraccion, intensa(Coef)),
    Coef > 10.

esMontaniaPeligrosa(Visitante, Atraccion):-
  atraccion(Atraccion, montaniaRusa(MaximosGirosInvertidos, _)),
  esAdulto(Visitante),
  not(estadoDeBienestar(Visitante, necesitaEntretenerse)),
  forall(atraccion(_, montaniaRusa(GirosInvertidos, _)), GirosInvertidos =< MaximosGirosInvertidos).

esAdulto(Visitante):-
    not(esChico(Visitante)).


opcionesDeEntretenimiento(Visitante, Comida):-
    puedeComprar(Visitante, Comida).

opcionesDeEntretenimiento(Visitante, Atraccion):-
    atraccion(Atraccion, Tipo),
    puedeAcceder(Visitante, Tipo).

opcionesDeEntretenimiento(Visitante, Atraccion):-
    atraccion(Atraccion, _),
    not(esMontaniaPeligrosa(Visitante, Atraccion)).

opcionesDeEntretenimiento(Visitante, Atraccion):-
    atraccion(Atraccion, _),
    not(esIntensa(Atraccion)).

opcionesDeEntretenimiento(Visitante, Atraccion, Mes):-
    atraccion(Atraccion, acuatica),
    mesDeApertura(Mes).


puedeAcceder(_, tranquila(chicosYAdultos)).
puedeAcceder(Visitante, tranquila(chicos)):-
    esChico(Visitante).




esMayorQue(_, montaniaRusa(MaximosGirosInvertidos, _)):-
    forall(atraccion(_,montaniaRusa(GirosACompararConMaximosGirosInvertidos)), MaximosGirosInvertidos > GirosACompararConMaximosGirosInvertidos).







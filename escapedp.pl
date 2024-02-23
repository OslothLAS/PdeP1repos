%persona(Apodo, Edad, Peculiaridades).
persona(ale, 15, [claustrofobia, cuentasRapidas, amorPorLosPerros]).
persona(agus, 25, [lecturaVeloz, ojoObservador, minuciosidad]).
persona(fran, 30, [fanDeLosComics]).
persona(rolo, 12, []).


%esSalaDe(NombreSala, Empresa).
esSalaDe(elPayasoExorcista, salSiPuedes).
esSalaDe(socorro, salSiPuedes).
esSalaDe(linternas, elLaberintoso).
esSalaDe(guerrasEstelares, escapepepe).
esSalaDe(fundacionDelMulo, escapepepe).

%terrorifica(CantidadDeSustos, EdadMinima).
%familiar(Tematica, CantidadDeHabitaciones).
%enigmatica(Candados).

%sala(Nombre, Experiencia).
sala(elPayasoExorcista, terrorifica(100, 18)).
sala(socorro, terrorifica(20, 12)).
sala(linternas, familiar(comics, 5)).
sala(guerrasEstelares, familiar(futurista, 7)).
sala(fundacionDelMulo, enigmatica([combinacionAlfanumerica, deLlave, deBoton])).


dificultad(Sala, Dificultad):-
    sala(Sala, terrorifica(Sustos, Edad)),
    Dificultad is Sustos - Edad.

dificultad(Sala, 15):-
    sala(Sala, familiar(futurista,_)).

dificultad(Sala, Dificultad):-
    sala(Sala, familiar(_,Dificultad)),
    not(esFuturista(Sala)).

dificultad(Sala, Dificultad):-
    sala(Sala, enigmatica(Candados)),
    length(Candados, Dificultad).

esFuturista(Sala):-
    sala(Sala, familiar(futurista,_)).



%Punto 2

puedeSalir(Persona, Sala):-
    puedeSalirDeUnaSala(Persona, Sala),
    not(esClaustrofibica(Persona)).

esClaustrofibica(Persona):-
    persona(Persona,_,Peculiaridades),
    member(claustrofobia, Peculiaridades).

puedeSalirDeUnaSala(_, Sala):-
    dificultad(Sala, 1).

puedeSalirDeUnaSala(Persona, Sala):-
    dificultad(Sala, Dificultad),
    esMayor(Persona),
    Dificultad < 5.

esMayor(Persona):-
    persona(Persona, Edad,_),
    Edad > 17.

%Punto 3

tieneSuerte(Persona, Sala):-
    persona(Persona,_,_),
    noTienePeculiaridad(Persona),
    puedeSalir(Persona, Sala).

noTienePeculiaridad(Persona):-
    persona(Persona,_,[]).

esMacabra(Empresa):-
    esSalaDe(_,Empresa),
    forall(esSalaDe(Sala, Empresa), esTerrorifica(Sala)).

esTerrorifica(Sala):-
    sala(Sala, terrorifica(_,_)).

esCopada(Empresa):-
    esSalaDe(_,Empresa),
    not(esMacabra(Empresa)),
    promedioMenorA(Empresa, 4).

promedioMenorA(Empresa,Valor):-
    findall(Dificultad , (esSalaDe(Sala, Empresa), dificultad(Sala, Dificultad)) , Dificultades),
    length(Dificultades, CantidadDeElementos),
    sum_list(Dificultades, SumaDeDificultades),
    Valor < SumaDeDificultades / CantidadDeElementos.



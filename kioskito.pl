

atiende(dodain, lunes, 9, 15).
atiende(dodain, miercoles, 9, 15).
atiende(dodain, viernes, 9, 15).
atiende(lucas, martes, 10, 20).
atiende(juanC, sabado, 18, 22).
atiende(juanC, domingo, 18, 22).
atiende(juanFsD, jueves, 10, 20).
atiende(juanFsD, viernes, 12, 20).
atiende(leoC, lunes, 14, 18).
atiende(leoC, miercoles, 14 ,18).
atiende(martu, miercoles, 23, 24).


atiende(vale, Dia, Entrada, Salida):-
    atiende(dodain, Dia, Entrada, Salida).

atiende(vale, Dia, Entrada, Salida):-
    atiende(juanC, Dia, Entrada, Salida).

atiende(nadie, Dia, Entrada, Salida):-
    atiende(leoC, Dia, Entrada, Salida).


quienAtiende(Kiosquero, Dia, Horario):-
    atiende(Kiosquero, Dia, Entrada, Salida),
    between(Entrada, Salida, Horario).

foreverAlone(Kiosquero, Dia, Horario):-
    quienAtiende(Kiosquero, Dia, Horario),
    estaSolo(Dia, Horario).

estaSolo(Dia, Horario):-
    findall(Kiosquero ,quienAtiende(Kiosquero, Dia, Horario), Kiosqueros),
    length(Kiosqueros, 1).

puedeAtender(Kiosquero, Dia):-
    quienAtiende(_, Dia, Horario),
    quienAtiende(Kiosquero, Dia, Horario).


% Queremos agregar las siguientes cláusulas:
% dodain hizo las siguientes ventas el lunes 10 de agosto: golosinas por $ 1200, cigarrillos Jockey, golosinas por $ 50
venta(dodain, fecha(10, 8), [golosinas(1200), cigarrillos(jockey), golosinas(50)]).
% dodain hizo las siguientes ventas el miércoles 12 de agosto: 8 bebidas alcohólicas, 
% 1 bebida no-alcohólica, golosinas por $ 10
venta(dodain, fecha(12, 8), [bebidas(true, 8), bebidas(false, 1), golosinas(10)]).
% martu hizo las siguientes ventas el miercoles 12 de agosto: golosinas por $ 1000, cigarrillos Chesterfield, Colorado y Parisiennes.
venta(martu, fecha(12, 8), [golosinas(1000), cigarrillos([chesterfield, colorado, parisiennes])]).
% lucas hizo las siguientes ventas el martes 11 de agosto: golosinas por $ 600.
venta(lucas, fecha(11, 8), [golosinas(99)]).
% lucas hizo las siguientes ventas el martes 18 de agosto: 2 bebidas no-alcohólicas y cigarrillos Derby.
venta(lucas, fecha(18, 8), [bebidas(true, 7), cigarrillos([derby])]).


ventaImportante(Kiosquero):-
    atiende(Kiosquero,_,_,_),
    forall(venta(Kiosquero, _, Ventas), primeraVentaEsImportante(Ventas)).

esImportante(golosinas(Valor)):-
    Valor > 100.

esImportante(cigarillos(Marcas)):-
    length(Marcas, Cantidad),
    Cantidad > 2.

esImportante(bebidas(true, Cantidad)):-
    Cantidad > 5.

primeraVentaEsImportante(Ventas):-
    primerVenta(Ventas, Venta),
    esImportante(Venta).


primerVenta([X|_], X).

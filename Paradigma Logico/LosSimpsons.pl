% Personajes qe son padres 
padreDe(abe, abbie).
padreDe(abe, homero).
padreDe(abe, herbert).
padreDe(clancy, marge).
padreDe(clancy, patty).
padreDe(clancy, selma).
padreDe(homero, bart).
padreDe(homero, hugo).
padreDe(homero, lisa).
padreDe(homero, maggie).

% Personajes que son madres
madreDe(edwina, abbie).
madreDe(mona, homero).
madreDe(gaby, herbert).
madreDe(jacqueline, marge).
madreDe(jacqueline, patty).
madreDe(jacqueline, selma).
madreDe(marge, bart).
madreDe(marge, hugo).
madreDe(marge, lisa).
madreDe(marge, maggie).
madreDe(selma, ling).

% Punto 1
tieneHijo(Personaje):-
    padreDe(Personaje, _).

tieneHijo(Personaje):-
    madreDe(Personaje, _).

% Punto 2
hermanos(Personaje1, Personaje2):-
    padreDe(Padre, Personaje1),
    padreDe(Padre, Personaje2),
    madreDe(Madre, Personaje1),
    madreDe(Madre, Personaje2),
    Personaje1 \= Personaje2.

% Punto 3
medioHermanos(Personaje1, Personaje2):-
    padreDe(Padre, Personaje1),
    padreDe(Padre, Personaje2);
    madreDe(Madre, Personaje1),
    madreDe(Madre, Personaje2),
    Personaje1 \= Personaje2.
    

:- (dynamic[fedor_em/2, brisa_em/2, sem_fedor_em/2, sem_brisa_em/2]).
:- (multifile[cacador/3, visitado/2, direcao/3, percepcoes/1, tem_ouro/1, vizinhos/1, vizinhos/2]).

% ---------------------------- %
% Regras de inferencia         %
% ---------------------------- %
sente_perigo(sim) :-
    percepcoes([sim, _, _, _, _]), !.
sente_perigo(sim) :-
    percepcoes([_, sim, _, _, _]), !.
sente_perigo(nao).

tem_buraco(X, Y, sim) :-
    (   L is X+1,
        N is Y+1,
        brisa_em(L, Y),
        brisa_em(X, N), !
    ;   N is Y+1,
        O is X-1,
        brisa_em(X, N),
        brisa_em(O, Y), !
    ;   O is X-1,
        S is Y-1,
        brisa_em(O, Y),
        brisa_em(X, S), !
    ;   S is Y-1,
        L is X+1,
        brisa_em(X, S),
        brisa_em(L, Y), !
    ).
tem_buraco(X, Y, talvez) :-
    L is X+1,
    \+ sem_brisa_em(L, Y),
    N is Y+1,
    \+ sem_brisa_em(X, N),
    O is X-1,
    \+ sem_brisa_em(O, Y),
    S is Y-1,
    \+ sem_brisa_em(X, S), !.
tem_buraco(_, _, nao).

tem_wumpus(X, Y, sim) :-
    (   L is X+1,
        N is Y+1,
        fedor_em(L, Y),
        fedor_em(X, N), !
    ;   N is Y+1,
        O is X-1,
        fedor_em(X, N),
        fedor_em(O, Y), !
    ;   O is X-1,
        S is Y-1,
        fedor_em(O, Y),
        fedor_em(X, S), !
    ;   S is Y-1,
        L is X+1,
        fedor_em(X, S),
        fedor_em(L, Y), !
    ).
tem_wumpus(X, Y, talvez) :-
    L is X+1,
    \+ sem_fedor_em(L, Y),
    N is Y+1,
    \+ sem_fedor_em(X, N),
    O is X-1,
    \+ sem_fedor_em(O, Y),
    S is Y-1,
    \+ sem_fedor_em(X, S), !.
tem_wumpus(_, _, nao).

% Agrega todo o conhecimento sobre Fedor e Brisa.
agrega_conhecimento(brisa) :-
    cacador(X, Y, _),
    assertz(brisa_em(X, Y)).
agrega_conhecimento(sem_brisa) :-
    cacador(X, Y, _),
    assertz(sem_brisa_em(X, Y)).
agrega_conhecimento(fedor) :-
    cacador(X, Y, _),
    assertz(fedor_em(X, Y)).
agrega_conhecimento(sem_fedor) :-
    cacador(X, Y, _),
    assertz(sem_fedor_em(X, Y)).

% ---------------------------- %
% Define heuristicas           %
% ---------------------------- %

% Caracteristicas da lista de sensores:
% [Fedor, Brisa, Brilho, Parede, Grito]

heuristica([_, _, _, _, _], exit) :-
    cacador(1, 1, _),
    tem_ouro(sim), !.

heuristica([_, _, sim, _, _], pegar) :- !.

heuristica([sim, _, _, _, _], [mover, X, Y]) :-
    write('O Wumpus está por aqui. '),
    agrega_conhecimento(fedor),
    agrega_conhecimento(sem_brisa),
    melhor_caminho(X, Y), !.

heuristica([_, sim, _, _, _], [mover, X, Y]) :-
    write('Tem um buraco por aqui. '),
    agrega_conhecimento(brisa),
    agrega_conhecimento(sem_fedor),
    melhor_caminho(X, Y), !.

heuristica([sim, sim, _, _, _], [mover, X, Y]) :-
    write('Tem Wumpus e buraco por aqui. '),
    agrega_conhecimento(fedor),
    agrega_conhecimento(brisa),
    melhor_caminho(X, Y), !.

% Volta pro comeco
heuristica(_, [mover, X, Y]) :-
    tem_ouro(sim),
    write('Voltando... '),
    melhor_caminho(X, Y), !.

% Exit if have found the same place ten times
heuristica(_, exit) :-
    cacador(X, Y, _),
    findall(1, visitado(X, Y), V),
    length(V, L),
    L>200,
    write('Eu desisto ').

heuristica(_, [mover, X, Y]) :-
    melhor_caminho(X, Y), !.

melhor_caminho(X, Y) :-
    % Calcula o custo de cada vizinho
    findall(C, custo_vizinho(C), L),
    min_list(L, Min),
    % Retorna o menor custo possivel
    index_of(L, Min, I),
    vizinhos(N),
    nth0(I, N, [X, Y]),
    format('~n> Custos ~p para ~p, escolho ~p~n',
           [L, N, Min]).

custo_vizinho(C) :-
    vizinhos(X, Y),
    soma_custo(X, Y, C).

% Predicados de custo
soma_custo(X, Y, C) :-
    findall(Ci, custo(X, Y, Ci), Cs),
    sum_list(Cs, C).

custo(X, Y, C) :-
    cacador(_, _, Di),
    direcao(X, Y, D),
    Di\==D,
    C is 1.
custo(X, Y, C) :-
    \+ visitado(X, Y),
    tem_buraco(X, Y, sim),
    C is 100.
custo(X, Y, C) :-
    \+ visitado(X, Y),
    tem_wumpus(X, Y, sim),
    C is 100.
custo(X, Y, C) :-
    \+ visitado(X, Y),
    sente_perigo(sim),
    tem_buraco(X, Y, talvez),
    C is 10.
custo(X, Y, C) :-
    \+ visitado(X, Y),
    sente_perigo(sim),
    tem_wumpus(X, Y, talvez),
    C is 10.
custo(X, Y, C) :-
    visitado(X, Y),
    tem_ouro(sim),
    C is (X-1)^2+(Y-1)^2-10, !.
custo(X, Y, C) :-
    visitado(X, Y),
    tem_ouro(nao),
    C is 5, !.

% Get the first occurence of certain value
index_of([H|_], H, 0) :- !.
index_of([_|T], H, Index) :-
    index_of(T, H, OldIndex), !,
    Index is OldIndex+1.

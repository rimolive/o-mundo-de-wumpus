:- (dynamic[fedor_em/2, brisa_em/2, sem_fedor_em/2, sem_brisa_em/2, virar/1]).
:- (multifile[cacador/3, visitado/2, direcao/3, percepcoes/1, tem_ouro/1, vizinhos/1, vizinhos/2, nas_fronteiras/2]).

% ---------------------------- %
% Regras de inferencia         %
% ---------------------------- %
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
agrega_conhecimento(fedor) :-
    cacador(X, Y, _),
    assertz(fedor_em(X, Y)).

% ---------------------------- %
% Define heuristicas           %
% ---------------------------- %

% Caracteristicas da lista de sensores:
% [Fedor, Brisa, Brilho, Parede, Grito]

heuristica([_, _, _, _, _], exit) :-
    cacador(1, 1, _),
    tem_ouro(sim), !.

heuristica([_, _, sim, _, _], pegar) :- !.

heuristica([sim, _, _, _, _], [virar, E]) :-
    (
        cacador(X, Y, D), D == leste, N is Y+1, nas_fronteiras(X, N), L is X+1, tem_fedor(X, N, nao), tem_fedor(L, Y, talvez);
        cacador(X, Y, D), D == norte, O is X-1, nas_fronteiras(O, Y), N is Y+1, tem_fedor(O, Y, nao), tem_fedor(X, N, talvez);
        cacador(X, Y, D), D == oeste, S is Y-1, nas_fronteiras(X, S), O is X-1, tem_fedor(X, S, nao), tem_fedor(O, Y, talvez);
        cacador(X, Y, D), D == sul, L is X+1, nas_fronteiras(L, Y), S is Y-1, tem_fedor(L, Y, nao), tem_fedor(X, S, talvez)
    ),
    write('O Wumpus está por aqui. '),
    agrega_conhecimento(fedor), !.

heuristica([sim, _, _, _, _], [virar, D]) :-
    (
        cacador(X, Y, D), D == leste, S is Y-1, nas_fronteiras(X, S), L is X+1, tem_fedor(X, S, nao), tem_fedor(L, Y, talvez);
        cacador(X, Y, D), D == norte, L is X+1, nas_fronteiras(L, Y), N is Y+1, tem_fedor(L, Y, nao), tem_fedor(X, N, talvez);
        cacador(X, Y, D), D == oeste, N is Y+1, nas_fronteiras(X, N), O is X-1, tem_fedor(X, N, nao), tem_fedor(O, Y, talvez);
        cacador(X, Y, D), D == sul, O is X-1, nas_fronteiras(O, Y), S is Y-1, tem_fedor(O, Y, nao), tem_fedor(X, O, talvez)
    ),
    write('O Wumpus está por aqui. '),
    agrega_conhecimento(fedor), !.

heuristica([sim, _, _, _, _], adiante) :-
    (
        cacador(X, Y, D), D == leste, L is X+1, nas_fronteiras(L, Y), tem_fedor(L, Y, nao);
        cacador(X, Y, D), D == norte, N is Y+1, nas_fronteiras(X, N), tem_fedor(X, N, nao);
        cacador(X, Y, D), D == oeste, O is X-1, nas_fronteiras(O, Y), tem_fedor(O, Y, nao);
        cacador(X, Y, D), D == sul, S is Y-1, nas_fronteiras(X, S), tem_fedor(X, S, nao)
    ),
    write('O Wumpus está por aqui. '),
    agrega_conhecimento(fedor), !.

heuristica([_, sim, _, _, _], [virar, esquerda]) :-
    (
        cacador(X, Y, D), D == leste, N is Y+1, nas_fronteiras(X, N), L is X+1, tem_buraco(X, N, nao), \+visitado(L, Y), tem_buraco(L, Y, talvez);
        cacador(X, Y, D), D == norte, O is X-1, nas_fronteiras(O, Y), N is Y+1, tem_buraco(O, Y, nao), \+visitado(X, N), tem_buraco(X, N, talvez);
        cacador(X, Y, D), D == oeste, S is Y-1, nas_fronteiras(X, S), O is X-1, tem_buraco(X, S, nao), \+visitado(O, Y), tem_buraco(O, Y, talvez);
        cacador(X, Y, D), D == sul, L is X+1, nas_fronteiras(L, Y), S is Y-1, tem_buraco(L, Y, nao), \+visitado(X, S), tem_buraco(X, S, talvez)
    ),
    write('Tem um buraco por aqui. '),
    agrega_conhecimento(brisa), !.

heuristica([_, sim, _, _, _], [virar, direita]) :-
    (
        cacador(X, Y, D), D == leste, S is Y-1, nas_fronteiras(X, S), L is X+1, tem_buraco(X, S, nao), \+visitado(L, Y), tem_buraco(L, Y, talvez);
        cacador(X, Y, D), D == norte, L is X+1, nas_fronteiras(L, Y), N is Y+1, tem_buraco(L, Y, nao), \+visitado(X, N), tem_buraco(X, N, talvez);
        cacador(X, Y, D), D == oeste, N is Y+1, nas_fronteiras(X, N), O is X-1, tem_buraco(X, N, nao), \+visitado(O, Y), tem_buraco(O, Y, talvez);
        cacador(X, Y, D), D == sul, O is X-1, nas_fronteiras(O, Y), S is Y-1, tem_buraco(O, Y, nao), \+visitado(X, S), tem_buraco(X, S, talvez)
    ),
    write('Tem um buraco por aqui. '),
    agrega_conhecimento(brisa), !.

heuristica([_, sim, _, _, _], adiante) :-
    (
        cacador(X, Y, D), D == leste, L is X+1, nas_fronteiras(L, Y), tem_buraco(L, Y, nao);
        cacador(X, Y, D), D == norte, N is Y+1, nas_fronteiras(X, N), tem_buraco(X, N, nao);
        cacador(X, Y, D), D == oeste, O is X-1, nas_fronteiras(O, Y), tem_buraco(O, Y, nao);
        cacador(X, Y, D), D == sul, S is Y-1, nas_fronteiras(X, S), tem_buraco(X, S, nao)
    ),
    write('Tem um buraco por aqui. '),
    agrega_conhecimento(brisa), !.

heuristica([_, _, _, sim, _], [virar, direita]) :- !.

heuristica([_, _, _, sim, _], [virar, esquerda]) :- !.


% Volta pro comeco
heuristica(_, [mover, X, Y]) :-
    tem_ouro(sim),
    write('Voltando... '),
    % melhor_caminho(X, Y),
    !.

heuristica(_, exit) :-
    cacador(X, Y, _),
    findall(1, visitado(X, Y), V),
    length(V, L),
    L>200,
    write('Eu desisto ').

heuristica(_, adiante).
% --------------------------------------------------------------------------- %
% Predicado do mundo                                                          %
% --------------------------------------------------------------------------- %

% Define um mundo de 4x4.
mundo(4, 4).

% --------------------------------------------------------------------------- %
% Predicados dos sujeitos                                                     %
% --------------------------------------------------------------------------- %
ouro(2, 3).
wumpus(1, 3).
buraco(3, 1).
buraco(3, 3).
buraco(4, 4).

% --------------------------------------------------------------------------- %
% Resultado da insercao dos predicados anteriores                             %
%     +---+---+---+---+                                                       %
%   4 |   |   |   | P |                                                       %
%     +---+---+---+---+                                                       %
%   3 | W | G | P |   |                                                       %
%     +---+---+---+---+                                                       %
%   2 |   |   |   |   |                                                       %
%     +---+---+---+---+                                                       %
%   1 | H |   | P |   |                                                       %
%     +---+---+---+---+                                                       %
%       1   2   3   4                                                         %
% --------------------------------------------------------------------------- %
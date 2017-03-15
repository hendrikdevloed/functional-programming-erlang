-module(ch3_5).
-export([doubleAll/1, evens/1, product/1]).
% Using higher-order functions
% Define the functions doubleAll, evens, and product using the higher-order functions lists:map, lists:filter and lists:foldr.

% doubleAll([]) -> [];
% doubleAll([X|Xs]) ->
%     [ 2*X | doubleAll(Xs) ].

doubleAll(Xs) -> lists:map(fun double/1, Xs).
double(X) -> 2*X.

% evens([]) -> [];
% evens([X|Xs]) when X rem 2 == 0 ->
%     [X | evens(Xs) ];
% evens([_|Xs]) ->
%     evens(Xs).
evens(Xs) -> lists:filter(fun is_even/1, Xs).
is_even(X) -> X rem 2 == 0.

% product([]) -> 1;
% product([X|Xs]) -> X * product(Xs).
product(Xs) -> lists:foldr(fun mult/2, 1, Xs).
mult(X,Y) -> X*Y.

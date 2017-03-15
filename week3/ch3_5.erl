-module(ch3_5).
-export([doubleAll/1, evens/1, product/1, zip/2, zip_with/3, zip_with_redef/3, zip_redef/2]).
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

% Zipping
% a) Define a function zip/2 that “zips together” pairs of elements from two lists like this:
% zip([1,3,5,7], [2,4]) = [ {1,2}, {3,4} ]
% where you can see that the elements from the longer list are lost.

zip(Xs,Ys) -> lists:reverse(zip(Xs, Ys, [])).
zip([],_,Result) -> Result;
zip(_,[],Result) -> Result;
zip([X|Xs], [Y|Ys], Result) -> zip(Xs,Ys,[{X,Y}|Result]).

% b) Define a function zip_with/3 that “zips together” pairs of elements from two lists using the function in the first argument, like this:
% zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) = [ 3, 7 ]
zip_with(F,Xs,Ys) -> lists:reverse(zip_with(F, Xs, Ys, [])).
zip_with(_,[],_,Result) -> Result;
zip_with(_,_,[],Result) -> Result;
zip_with(F, [X|Xs], [Y|Ys], Result) -> zip_with(F, Xs, Ys, [F(X,Y)|Result]).

% c) Re-define the function zip_with/3 using zip and lists:map.
zip_with_redef(F, Xs, Ys) -> lists:map(fun({X,Y}) -> F(X,Y) end, zip(Xs,Ys)).

% d) Re-define zip/2 using zip_with/3.
zip_redef(Xs, Ys) -> zip_with(fun(X,Y)->{X,Y} end, Xs, Ys).

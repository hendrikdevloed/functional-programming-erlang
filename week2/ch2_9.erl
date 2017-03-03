-module(ch2_9).
-export([double/1, evens/1, median/1]).

double([]) -> [];
double([X|Xs]) -> [X,X|double(Xs)].

evens([]) -> [];
evens([X|Xs]) when X rem 2 == 0 -> [X|evens(Xs)];
evens([_|Xs]) -> evens(Xs).

median([X]) -> X;
median([A,B]) -> (A+B)/2;
median([_|Xs]) -> median(init(Xs)).

% returns initial part of list: all except last element
init([]) -> [];
init([_]) -> [];
init([X|Xs]) -> [X|init(Xs)].

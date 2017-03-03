-module(ch2_6).
-export([product/1,maximum/1]).

product(Xs) -> product(Xs, 1).
product([],P) -> P;
product([X|Xs],P) -> product(Xs, X*P).

maximum([X|Xs]) -> maximum(Xs, X).
maximum([], M) -> M;
maximum([X|Xs], M) -> maximum(Xs, max(X,M)).

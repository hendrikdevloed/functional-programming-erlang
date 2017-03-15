-module(rps).
-export([result/2]).

beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

lose(rock) -> scissors;
lose(scissors) -> paper;
lose(paper) -> rock.

result(Move,Counter) ->
    case {beat(Move), lose(Move)} of
        {Counter,_} -> lose;
        {_,Counter} -> win;
        _ -> draw
    end.

% A tournament is a series of rounds – each round is a single choice from the two players,
% which we’ll call left and right. Suppose that the choices are given as two lists; give the
% tournament result as an integer, so that the number counts the difference between the number
% of wins for left and right. A positive value is an overall win for left, a negative for right,
% and zero represents an overall draw. For instance:
% tournament([rock,rock,paper,paper],[rock,paper,scissors,rock] = -1
% Which higher-order functions from the lists module can you use in computing this solution?

% => 
%    lists:zip to join individual moves from both lists into a pair of moves for a single game
%    lists:map to evaluate the result of each game individually
%    lists:map to convert a win/lose/draw result into a numeric score
%    lists:foldr to coalesce all individual scores into a final score,

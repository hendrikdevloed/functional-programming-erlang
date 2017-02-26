-module(week1).
-export([perimeter/1, area/1, enclose/1, bitsDirect/1, bitsTail/1, bits/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Shapes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define a function perimeter/1 which takes a shape and returns the perimeter of the shape.
% Choose a suitable representation of triangles, and augment area/1 and perimeter/1 to handle this case too.
perimeter({circle, R}) ->
    math:pi() * 2.0 * R;
perimeter({rectangle, W, H}) ->
    2.0 * (W + H);
perimeter({triangle, A, B, C}) ->
    A + B + C.

area({circle, R}) ->
    math:pi() * R * R;
area({rectangle, W, H}) ->
    W * H;
area({triangle, A, B, C}) ->
    heron(A, B, C).

% Heron's formula for the area of a triangle given the length of its sides
heron(A, B, C) ->
    S = perimeter({triangle, A, B, C}) / 2.0,
    math:sqrt( S * (S-A) * (S-B) * (S-C) ).

% Define a function enclose/1 that takes a shape an returns the smallest enclosing rectangle of the shape.
enclose({circle, R}) ->
    {rectangle, 2.0*R, 2.0*R};
enclose({rectangle, W, H}) ->
    {rectangle, W, H};

% https://en.wikipedia.org/wiki/Minimum_bounding_box_algorithms#cite_note-1
% "a side of a minimum-area enclosing box must be collinear with a side of the convex polygon"
% So we could brute-force
%
% enclose({triangle, A, B, C}) ->
%    RA = {rectangle, A, triangleHeight(A,B,C)},
%    RB = {rectangle, B, triangleHeight(B,A,C)},
%    RC = {rectangle, C, triangleHeight(C,A,B)},
%    minRect(RA,RB,RC).
%
% Given that heron(Base, S1, S2) = area = Base * Height / 2 we can deduce the
% height of the triangle as
% triangleHeight(Base, S1, S2) -> 2.0 * heron(Base, S1, S2) / Base.
% BUT...
% Noting that if we make the rectangle base collinear with a base of the triangle and the rectangle height
% matches the height of the corresponding triangle from that base, we can conclude that the area of the
% enclosing rectangle will *always* be twice the area of the triangle,
% so there is no need to brute-force anything and we can take any side
% as the base of the minimum enclosing rectangle.

enclose({triangle, A, B, C}) ->
    {rectangle, A, triangleHeight(A,B,C)}.

triangleHeight(Base, S1, S2) -> 2.0 * heron(Base, S1, S2) / Base.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Summing the bits
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define a function bits/1 that takes a positive integer N and returns the sum of the bits in the binary representation.
% For example bits(7) is 3 and bits(8) is 1.
% See whether you can make both a direct recursive and a tail recursive definition.
% Which do you think is better? Why?

bitsDirect(0) -> 0;
bitsDirect(1) -> 1;
bitsDirect(N) -> bitsDirect(N div 2) + bitsDirect(N rem 2).

% Naive tail recursive approach:
bitsTail(N) -> bitsTail(N,0).
bitsTail(0, Sum) -> Sum;
bitsTail(1, Sum) -> Sum+1;
bitsTail(N,Sum) ->
    LowBit = N rem 2,
    OtherBits = N div 2,
    bitsTail(OtherBits, Sum+LowBit).
% The direct approach is much more readable from an algorithmic approach (states clearly what it does),
% the tail-recursive approach is more burdoned with implementation details (the Sum accumlator).
% From a performance point of view the tail-recursive approach wins because it does not build up a O(log(N))
% backlog of postponed additions.
% An approach using mutual recursion mixes the best of both worlds.
bits(0) -> 0;
bits(1) -> 1;
bits(N) -> bits(N, 0).

bits(0, Sum) -> Sum;
bits(N, Sum) -> bits(N div 2, Sum + bits(N rem 2)).

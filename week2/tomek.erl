-module(tomek). 
-export([get_file_contents/1,show_file_contents/1, to_words/1, unique/1,
normalize/1, find_word_info/2, update_word_info/2, indexed_lines/1, 
build_index/1, build_index/2, update_index/3, update_index_for_word/3]).

% Used to read a file into a list of lines. 
% Example files available in: 
% gettysburg-address.txt (short) 
% dickens-christmas.txt (long) 
% Get the contents of a text file into a list of lines. 
% Each line has its trailing newline removed.

get_file_contents(Name) -> 
{ok,File} = file:open(Name,[read]), 
Rev = get_all_lines(File,[]), 
lists:reverse(Rev).

% Auxiliary function for get_file_contents. 
% Not exported.

get_all_lines(File,Partial) -> 
case io:get_line(File,"") of 
eof -> file:close(File), 
Partial; 
Line -> {Strip,_} = lists:split(length(Line)-1,Line), 
get_all_lines(File,[Strip|Partial]) 
end.

% Show the contents of a list of strings. 
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) -> 
io:format("~s~n",[L]), 
show_file_contents(Ls); 
show_file_contents([]) -> 
ok.

%-------------------------------------------------- 
% Solution starts here 
% -------------------------------------------------

% normalization functions 
normalize(W) -> string:to_lower(W). 
to_words(L) -> string:tokens(normalize(L), " .-!?:\\"). 
unique(Ws) -> lists:usort(Ws).

% combine lines with corresponding numbers in tuples 
% like this: [{1, "first line"}, {2, "another line"}] 
indexed_lines([]) -> []; 
indexed_lines(Ls) -> lists:zip(lists:seq(1, length(Ls)), Ls).

% split index into two parts of which the second starts with 
% the word of interest 
find_word_info(W, Is) -> lists:splitwith(fun({X, _Ls}) -> X =/= W end, Is).

% update word entry based on given line numbers 
% e.g.: given line N = 3 and word info = {"ala", [{1,2}]} 
% this function will generate {"ala", [{1,3}]} 
update_word_info(N, {W, []}) -> {W, [{N, N}]}; 
update_word_info(N, {W, [{N1, N2}|Xs]}) when N2+1 >= N -> {W, [{N1, N}|Xs]}; 
update_word_info(N, {W,Xs}) -> {W, [{N, N}|Xs]}.

% update Index of words (a list in form: [{"word1", [{N1,N2},{...}]}, ...]) 
% with word W that occured in line L 
update_index_for_word(L, W, Index) -> 
{Head, Tail} = find_word_info(W, Index), 
case Tail of 
[] -> Head ++ [update_word_info(L, {W, []})]; 
[ExW| Ws] -> Head ++ [update_word_info(L, ExW)| Ws] 
end.

% update Index of words with all words from line L 
update_index(_L, [], Index) -> 
Index; 
update_index(L, [W|Ws], Index) -> 
update_index(L, Ws, update_index_for_word(L, W, Index)).

% build Index for all (indexed) lines Ls using given word 
% index 
build_index([], Index) -> 
Index; 
build_index([{N, L}|Ls], Index) -> 
Words = lists:filter(fun(W) -> length(W)>2 end, unique(to_words(L))), 
NewIndex = update_index(N, Words, Index), 
build_index(Ls, NewIndex).

% main function: build index of words for all lines 
% example usage: index:build_index(index:get_file_contents("gettysburg-address.txt")). 
% Note: actual index reversed (starting from lines closer to the end of file). 
% The algorithm is case insensitive, results are sorted, short words are removed 
% from the index. No other refinements were done. 
build_index(Ls) -> 
Index = build_index(indexed_lines(Ls), []), 
lists:sort(fun({W1,_},{W2,_}) -> W1 < W2 end, Index).


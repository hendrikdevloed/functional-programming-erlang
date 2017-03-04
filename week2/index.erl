-module(index).
-export([get_file_contents/1,show_file_contents/1,index/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Given
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Solution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For the purposes of this exercise, we define skippable punctuation as every non-letter
is_punctuation(Char) when (Char >= $a) and (Char =< $z) -> false;
is_punctuation(Char) when (Char >= $A) and (Char =< $Z) -> false;
is_punctuation(_) -> true.

% Split a string into words by breaking on and skipping over punctuation
get_all_words(Line) -> lists:reverse(get_all_words(Line,[],[])).

get_all_words([], [], Words) -> Words;
get_all_words([], Word, Words) -> [lists:reverse(Word)|Words];
get_all_words([Char|Rest], Word, Words) ->
    case is_punctuation(Char) of
        true -> case Word of
            [] -> get_all_words(Rest, [], Words);
            _ -> get_all_words(Rest, [], [lists:reverse(Word)|Words])
        end;
        false -> get_all_words(Rest, [Char|Word], Words)
    end.

% Remove all words of 3 character or less from a list of words
remove_trivial_words(Words) -> remove_trivial_words(Words,[]).

remove_trivial_words([],Result) -> Result;
remove_trivial_words([Word|Words], Result) ->
    case length(Word) =< 3 of
        true -> remove_trivial_words(Words, Result);
        false -> remove_trivial_words(Words, [Word|Result])
    end.


% convert a list of 
tag_linenumber(Lines) -> tag_linenumber(Lines, 1, []).

tag_linenumber([], _, Result) -> Result;
tag_linenumber([Line|Lines], LineNum, Result) ->
    case Line of
        [] -> tag_linenumber(Lines, LineNum+1, Result); % Skip empty lines while tagging
        Words ->
            TaggedWords = lists:map(fun(Word) -> {Word, LineNum} end, Words),
            tag_linenumber(Lines, LineNum+1, [TaggedWords|Result])
    end.

% convert page to page list and join identical words [{"aword",5},{"word",1},{"word",2}] into [{"aword",[5]}, {"word",[1,2]}]
nub_wordlist(WordList) -> lists:reverse(nub_wordlist(WordList, [], [], [])).

nub_wordlist([], [], [], Result) -> Result;
nub_wordlist([], LastWord, LastLines, Result) -> [{LastWord, lists:sort(LastLines)}|Result];
nub_wordlist([{Word, Line}|WordList], LastWord, LastLines, Result) ->
    case Word of
        LastWord -> nub_wordlist(WordList, LastWord, [Line|LastLines], Result);
        _ -> nub_wordlist(WordList, Word, [Line], [{LastWord, lists:sort(LastLines)}|Result])
    end.

% Convert individual lines [1,4,5,6,9] into [{1,1},{4,6},{9,9}]
page_range([]) -> [];
page_range([Line|Lines]) -> lists:reverse(page_range(Lines, {Line,Line}, [])).

page_range([], Range, Ranges) -> [Range|Ranges];
page_range([Line|Lines], {RangeFrom,RangeTo}, Ranges) when RangeTo+1==Line ->
    page_range(Lines, {RangeFrom,Line}, Ranges);
page_range([Line|Lines], Range, Ranges) ->
    page_range(Lines, {Line,Line}, [Range|Ranges]).


% Main function: load a file and generate an index
index(Filename) ->
    WordsOfLines = lists:map( % convert each line into a list of words
        fun(Line) ->
            remove_trivial_words( % remove all words =< 3 characters
                get_all_words( % split and ignore whitespace
                    string:to_lower(Line)
                )
            )
        end,
        get_file_contents(Filename)
    ),
    IndexedWords = nub_wordlist( % merge all {word,page} tuples into unique words with a page list
        lists:keysort(1, % sort by word
            lists:flatten( % coalesce word lists per lines [[],[],... ] into one huge list [{word, line}]
                tag_linenumber(WordsOfLines) % convert to [[{"someword",1},...,{"otherword",1}], [{"more",2}...]
            )
        )
    ),
    % Convert pages into {page,page} and ranges into {pagefrom,pageto}
    ClusteredIndex = lists:map(fun({Word,Lines}) -> {Word, page_range(Lines)} end, IndexedWords),
    % The result:
    ClusteredIndex.

-module(problems).

-export([last/1]).


last([]) ->
    nil;
last([Elem]) ->
    Elem;
last([_|Tail]) ->
    last(Tail).

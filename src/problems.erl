-module(problems).

-export([last/1, last_but_last/1]).

%% 1 %%

last([]) ->
    nil;
last([Elem]) ->
    Elem;
last([_|Tail]) ->
    last(Tail).

%% 2 %%

last_but_last([]) ->
  nil;
last_but_last([Head|[_]]) ->
    Head;
last_but_last([_|Tail]) ->
  last_but_last(Tail).

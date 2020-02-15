-module(problems).

-export([last/1, last_but_last/1, kth_elem/2]).

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

%% 3 %%

kth_elem([],N) when N > 0 -> error;
kth_elem(_,N) when N == 0 -> tooSmall;
kth_elem([Elem|_],N) when N == 1 -> Elem;
kth_elem([_|Tail],N) when N > 0 -> kth_elem(Tail, N-1).

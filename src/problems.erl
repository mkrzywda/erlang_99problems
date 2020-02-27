%% @author Maciej Krzywda <maciej.krzywdaa@gmail.com>
%%
%% @doc This is the 99 problems solution in Erlang.
%%
%% It provides list operations:
%% <li>last - Find the last element of a list,</li>
%% <li>last_but_last - Find the last but one element of a list,</li>
%% <li>kth_elem - Find the K'th element of a list,</li>
%% <li>len - Find the number of elements of a list,</li>
%% <li>my_reverse - Reverse a list,</li>
%% <li>palindrome - Find out whether a list is a palindrome,</li>
%% <li>flatten - Flatten a nested list structure,</li>
%% <li>compress - Eliminate consecutive duplicates of list elements,</li>
%% <li>pack - Eliminate consecutive duplicates of list elements.</li>

-module(problems).
% -export([last/1, last_but_last/1, kth_elem/2, len/1, my_reverse/1, palindrome/1 , flatten/1, compress/1, pack/1 ]).
-compile(export_all).

%% @doc Find the last element of a list.
last([]) ->
    nil;
last([Elem]) ->
    Elem;
last([_|Tail]) ->
    last(Tail).


%% @doc Find the last but one element of a list.
last_but_last([]) ->
  nil;
last_but_last([Head|[_]]) ->
    Head;
last_but_last([_|Tail]) ->
  last_but_last(Tail).

%% @doc Find the K'th element of a list.
kth_elem([],N) when N > 0 -> error;
kth_elem(_,N) when N == 0 -> tooSmall;
kth_elem([Elem|_],N) when N == 1 -> Elem;
kth_elem([_|Tail],N) when N > 0 -> kth_elem(Tail, N-1).

%% @doc Find the number of elements of a list.
len([]) ->
    0;
len([_|Tail]) ->
    1 + len(Tail).

%% @doc Reverse a list.
my_reverse(List) ->
    my_reverse(List,[]).
my_reverse([],Res) ->
    Res;
my_reverse([Head|Tail],Res) ->
    my_reverse(Tail,[Head|Res]).

%% @doc Find out whether a list is a palindrome
palindrome(List) ->
	palindrome(List, my_reverse(List)).
palindrome([], []) ->
	true;
palindrome([Head], [Head]) ->
	true;
palindrome([Head|Tail], [Head|Tail]) ->
	palindrome(Tail, Tail);
palindrome(_, _) ->
	false.

%% @doc Flatten a nested list structure
flatten(List) ->
	flatten(List, []).
flatten([], Res) ->
	my_reverse(Res);
flatten([Head|Tail], Res) when is_list(Head) ->
	flatten(Tail, my_reverse(flatten(Head, Res)));
flatten([Head|Tail], Res) ->
	flatten(Tail, [Head|Res]).


%% @doc Eliminate consecutive duplicates of list elements
compress(List) -> my_reverse(compress(List,[])).						
compress([Head], Res) 	-> [Head|Res];
compress([Head,Head|Tail],Res) when Head == Head
		       	       	-> compress([Head|Tail], Res);
compress([Head,Sec|Tail],Res) when not(Head == Sec)
				-> compress([Sec|Tail], [Head|Res]).

%% @doc Consecutive duplicates of list elements into sublists.
pack(List) -> my_reverse(pack(List,[])).

pack([],Res) 	-> Res;
pack(List,Res) 	-> [ResTmp,NewTail] = tmp_pack(List,[]),
			   pack(NewTail,[ResTmp]++Res).

tmp_pack([Head],Res) 	-> [[Head|Res],[]];
tmp_pack([Head,Second|Tail],Res) when not(Head == Second)
				-> [[Head|Res],[Second|Tail]];
tmp_pack([Head,Head|Tail], Res) when Head == Head 
				-> tmp_pack([Head|Tail],[Head|Res]).


%% @doc Run-length encoding of a list.
encode(List) -> 	NList = pack(List),
			my_reverse(encode(NList,[])).
encode([],Result) 		-> Result;
encode([Head|Tail],Result) 	-> [First|_] = Head,
				   Size = len(Head),
				   encode(Tail,[[Size,First]|Result]).

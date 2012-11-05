-module(lists2).

-export([ukeysublist/3, unique/1, group_with/2, keys/2, shuffle/1]).


%% @doc `TupleList1' and `TupleList2' are returned by `lists:ukeysort(N, _)'.
-spec ukeysublist(N, TupleList1, TupleList2) ->
    TupleList3 when
    N :: non_neg_integer(),
    TupleList2 :: TupleList1,
    TupleList3 :: TupleList1,
    TupleList1 :: [tuple()].

ukeysublist(N, [H1|T1]=L1, [H2|T2]=L2) ->
    E1 = element(N, H1),
    E2 = element(N, H2),
    if E1 < E2 -> [H1|ukeysublist(N, T1, L2)];
       E1 > E2 -> ukeysublist(N, L1, T2);
       true    -> ukeysublist(N, T1, T2)
    end;
ukeysublist(_N, L1, _L2) ->
    L1.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ukeysublist_test_() ->
    [?_assertEqual(ukeysublist(1, [{1}, {2}, {3}], [{2}]),
                   [{1}, {3}])
    ,?_assertEqual(ukeysublist(1, [{1}, {2}, {3}], [{0}, {4}]),
                   [{1}, {2}, {3}])
    ,?_assertEqual(ukeysublist(1, [{1}, {2}, {3}], [{0}, {1}, {3}, {4}]),
                   [{2}])
    ].

-endif.


%% @doc Delete duplicates and SAVE the current elements' order.
%% If the current order is not important, than use `lists:usort/1' instead.
unique(L) ->
    keys(2, lists:keysort(1, lists:ukeysort(2, enumerate(L)))).
    

%% @doc Create a list of pairs: `[{lists:nth(X), X}]'.
%%
%% Converts `[a, b, c]' to `[{1, a}, {2, b}, {3, c}]'.
enumerate(L) ->
    enumerate(L, 1).

enumerate([H|T], N) ->
    [{N, H} | enumerate(T, N+1)];
enumerate([], _N) ->
    [].


%% @doc Looks like `GROUP BY KeyMaker(List)` in SQL.
-spec group_with(fun(), list()) -> list({term(),list()}).

group_with([], _keymaker) ->
    [];

group_with(List, KeyMaker) ->
    %% Map
    Mapped = [{KeyMaker(X), X} || X <- List],
    [{SortedHKey, SortedHValue}|SortedT] = lists:keysort(1, Mapped),

    %% Reduce
    group_reduce(SortedT, SortedHKey, [SortedHValue]).
    

%% @doc Return `[{Key, [Value1, Value2, ...]}]'.
%% @end
%%
%% Still the same group:
group_reduce([{Key, Val}|T], Key, Vals) ->
    group_reduce(T, Key, [Val|Vals]);

%% Add a new group:
group_reduce([{Key, Val}|T], Key, Vals) ->
    [{Key, lists:reverse(Vals)} | group_reduce(T, Key, [Val])];

group_reduce([], Key, Vals) ->
    [{Key, lists:reverse(Vals)}].


%% group_with end


%% @doc Apply `element(N, _)' for each element.
keys(N, List) ->
    [element(N, X) || X <- List].


shuffle(List) -> 
    WithKey = [ {random:uniform(), X} || X <- List ],
    Sorted  = lists:keysort(1, WithKey),
    keys(2, Sorted).

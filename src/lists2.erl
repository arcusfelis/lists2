-module(lists2).

-export([ordkeysublist/3,
         ordkeysublist/4,
         ordkeymerge_with/4,
         ordkeymerge_with/5,
         unique/1,
         shuffle/1,
         group_with/2,
         group_count_with/2,
         map_group_with/2,
         group_pairs/1,
         seq_group_with/2,
         group_by/2,
         keys/2,
         elements/2,
         enumerate/1,
         enumerate/2,
         filter_head/2,
         cmap/2,
         cmap/3,
         collate_with/2,
         desc_collate_with/2,
         rotate/1,
         rotate/2,
         align_ordset/2,
         align_ordset/3,
         zip4/4,
         zip5/5,
         zip6/6,
         zip_with4/5,
         zip_with5/6,
         zip_with6/7,
         sorted_non_unique_elements/1,
         sorted_to_non_unique_elements/1,
         sorted_unique_elements/1,
         sorted_to_unique_elements/1]).



%% @doc It is like `lists:sublist/1', BUT uses a tuple field for comparation.
%% `TupleList1' and `TupleList2' must be sorted, for example, 
%% with `lists:ukeysort(N, _)'.
-spec ordkeysublist(N, TupleList1, TupleList2) ->
    TupleList3 when
    N :: non_neg_integer(),
    TupleList2 :: TupleList1,
    TupleList3 :: TupleList1,
    TupleList1 :: [tuple()].

ordkeysublist(N, [H1|T1]=L1, [H2|T2]=L2) ->
    E1 = element(N, H1),
    E2 = element(N, H2),
    if E1 < E2 -> [H1|ordkeysublist(N, T1, L2)];
       E1 > E2 -> ordkeysublist(N, L1, T2);
       true    -> ordkeysublist(N, T1, T2)
    end;
ordkeysublist(_N, L1, _L2) ->
    L1.


%% @doc Zip two sorted lists of tuples using an unique key in a field N.
%% `Zipper' will be called for each unique key.
%%
%% If the arity of `Zipper' is `2', then the function will be called as:
%% `Zipper(E1, E2)'.
%%
%% `E1' and `E2' are elements in `L1' and `L2' or `undefined', if there is no 
%% such value in the list.
%%
%% If the arity of `Zipper' is `3', then the function will be called as:
%% `Zipper(Key, E1, E2)'.
%% @end
-spec ordkeymerge_with(N, Zipper, L1, L2) -> L3 when
    L1 :: [E1],
    L2 :: [E2],
    L3 :: [E3],
    N  :: non_neg_integer(),
    Zipper :: Zipper2 | Zipper3,
    Zipper2 :: fun((E1 | undefined, E2 | undefined) -> E3),
    Zipper3 :: fun((Key, E1 | undefined, E2 | undefined) -> E3),
    Key :: term(),
    E1 :: term(),
    E2 :: term(),
    E3 :: term().

ordkeymerge_with(N, Zipper, L1, L2) ->
    ordkeymerge_with(N, N, Zipper, L1, L2).


-spec ordkeymerge_with(N1, N2, Zipper, L1, L2) -> L3 when
    L1 :: [E1],
    L2 :: [E2],
    L3 :: [E3],
    N1 :: non_neg_integer(),
    N2 :: non_neg_integer(),
    Zipper :: Zipper2 | Zipper3,
    Zipper2 :: fun((E1 | undefined, E2 | undefined) -> E3),
    Zipper3 :: fun((Key, E1 | undefined, E2 | undefined) -> E3),
    Key :: term(),
    E1 :: term(),
    E2 :: term(),
    E3 :: term().

ordkeymerge_with(N1, N2, Zipper, L1, L2) 
    when is_function(Zipper, 2), 
        is_integer(N1), N1 > 0, is_integer(N2), N2 > 0, is_list(L1), is_list(L2) ->
    ordkeymerge_with2(N1, N2, Zipper, L1, L2);

ordkeymerge_with(N1, N2, Zipper, L1, L2) 
    when is_function(Zipper, 3),
        is_integer(N1), N1 > 0, is_integer(N2), N2 > 0, is_list(L1), is_list(L2) ->
    ordkeymerge_with3(N1, N2, Zipper, L1, L2).


ordkeymerge_with2(N1, N2, Z, [H1|T1]=L1, [H2|T2]=L2) ->
    Key1 = element(N1, H1),
    Key2 = element(N2, H2),
    if
        Key1 =:= Key2 -> [Z(H1, H2) | ordkeymerge_with2(N1, N2, Z, T1, T2)];
        %% Keys1 = [1,2,3] Keys2 = [2,3]
        Key1 < Key2   -> [Z(H1, undefined) | ordkeymerge_with2(N1, N2, Z, T1, L2)];
        %% Keys1 = [2,3] Keys2 = [1,2,3]
        true          -> [Z(undefined, H2) | ordkeymerge_with2(N1, N2, Z, L1, T2)]
    end;
ordkeymerge_with2(_N1, _N2, Z, [], L2) ->
    [Z(undefined, H2) || H2 <- L2];
ordkeymerge_with2(_N1, _N2, Z, L1, []) ->
    [Z(H1, undefined) || H1 <- L1].


ordkeymerge_with3(N1, N2, Z, [H1|T1]=L1, [H2|T2]=L2) ->
    Key1 = element(N1, H1),
    Key2 = element(N2, H2),
    if
        Key1 =:= Key2 -> [Z(Key1, H1, H2) | ordkeymerge_with2(N1, N2, Z, T1, T2)];
        %% Keys1 = [1,2,3] Keys2 = [2,3]
        Key1 < Key2   -> [Z(Key1, H1, undefined) | ordkeymerge_with2(N1, N2, Z, T1, L2)];
        %% Keys1 = [2,3] Keys2 = [1,2,3]
        true          -> [Z(Key2, undefined, H2) | ordkeymerge_with2(N1, N2, Z, L1, T2)]
    end;
ordkeymerge_with3(_N1, N2, Z, [], L2) ->
    [Z(element(N2, H2), undefined, H2) || H2 <- L2];
ordkeymerge_with3(N1, _N2, Z, L1, []) ->
    [Z(element(N1, H1), H1, undefined) || H1 <- L1].


%% @doc Insert `undefined' on place of elements of `MasterSet' with values
%% from `ordsets:subtract(MasterSet,SrcSet)'.
%%
%% Result list is not an ordset.
-spec align_ordset(SrcSet, MasterSet) -> list() when
    SrcSet :: list(),
    MasterSet :: list().

align_ordset([H|T1], [H|T2])    -> [H|align_ordset(T1, T2)];
%% skip H1.
align_ordset(T1, [_H2|T2])      -> [undefined|align_ordset(T1, T2)];
align_ordset([], [])            -> [].



%% @doc Align `KeySet' with `MasterSet'. The result list contains values from
%% `ValSet'.
-spec align_ordset(KeySet, ValSet, MasterSet) -> list() when
    KeySet :: list(),
    ValSet :: list(),
    MasterSet :: list().

align_ordset([K|Ks], [V|Vs], [K|Ms]) -> [V|align_ordset(Ks, Vs, Ms)];
%% Skip K, V.
align_ordset(Ks, Vs, [_|Ms])         -> [undefined|align_ordset(Ks, Vs, Ms)];
align_ordset([], [], [])             -> [].


%% @doc It is a variant of `ordkeysublist/3', that uses different field positions.
-spec ordkeysublist(N1, TupleList1, N2, TupleList2) ->
    TupleList3 when
    N1 :: non_neg_integer(),
    N2 :: non_neg_integer(),
    TupleList2 :: TupleList1,
    TupleList3 :: TupleList1,
    TupleList1 :: [tuple()].

ordkeysublist(N1, [H1|T1]=L1, N2, [H2|T2]=L2) ->
    E1 = element(N1, H1),
    E2 = element(N2, H2),
    if E1 < E2 -> [H1|ordkeysublist(N1, T1, N2, L2)];
       E1 > E2 -> ordkeysublist(N1, L1, N2, T2);
       true    -> ordkeysublist(N1, T1, N2, T2)
    end;
ordkeysublist(_N1, L1, _N2, _L2) ->
    L1.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ordkeysublist_test_() ->
    [?_assertEqual(ordkeysublist(1, [{1}, {2}, {3}], [{2}]),
                   [{1}, {3}])
    ,?_assertEqual(ordkeysublist(1, [{1}, {2}, {3}], [{0}, {4}]),
                   [{1}, {2}, {3}])
    ,?_assertEqual(ordkeysublist(1, [{1}, {2}, {3}], [{0}, {1}, {3}, {4}]),
                   [{2}])
    ].

ordkeysublist4_test_() ->
    [?_assertEqual(ordkeysublist(1, [{1}, {2}, {3}], 1, [{2}]),
                   [{1}, {3}])
    ,?_assertEqual(ordkeysublist(1, [{1}, {2}, {3}], 1, [{0}, {4}]),
                   [{1}, {2}, {3}])
    ,?_assertEqual(ordkeysublist(1, [{1}, {2}, {3}], 1, [{0}, {1}, {3}, {4}]),
                   [{2}])
    ,?_assertEqual(ordkeysublist(1, [{1}, {1}, {2}, {3}], 1, [{0}, {1}, {3}, {4}]),
                   [{1}, {2}])
    ].

-endif.


%% @doc Delete duplicates and SAVE the current elements' order.
%% If the current order is not important, than use `lists:usort/1' instead.
unique([H|T]) ->
    [H|unique(delete_all(H, T))];
unique([]) ->
    [].

delete_all(X, [X|T]) -> delete_all(X, T);
delete_all(X, [H|T]) -> [H|delete_all(X, T)];
delete_all(_, []) -> [].


%% @doc Returns a list in random order.
shuffle(List) when is_list(List) -> 
    WithKey = [ {random:uniform(), X} || X <- List ],
    Sorted  = lists:keysort(1, WithKey),
    keys(2, Sorted).
    

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
%% Returns a list of `{Key, GroupOfElements}'.
%% `GroupOfElements' contains all elements of lists for each `Key = KeyMaker(_)'.
%%
%% For example:
%%
%% <code>
%% group_with(fun(X) -> X rem 2 end, [1,2,4,5,3]).
%% [{0, [2, 4]}, {1, [1, 3, 5]}]
%% </code>
-spec group_with(fun(), list()) -> list({term(),list()}).

group_with(_keymaker, []) ->
    [];

group_with(KeyMaker, [_|_] = List)  when is_function(KeyMaker, 1) ->
    %% Map
    Mapped = [{KeyMaker(X), X} || X <- List],
    [{SortedHKey, SortedHValue}|SortedT] = lists:keysort(1, Mapped),

    %% Reduce
    group_reduce(SortedT, SortedHKey, [SortedHValue]).


%% @doc Looks like `SELECT Value GROUP BY Key` in SQL.
-spec map_group_with(KeyValueMaker, list()) -> list({Key,[Value]}) when
    KeyValueMaker :: fun((Elem) -> {Key, Value}),
    Elem :: term(),
    Key :: term(),
    Value :: term().

map_group_with(_KeyValueMaker, []) ->
    [];

map_group_with(KeyValueMaker, [_|_] = List) when is_function(KeyValueMaker, 1) ->
    %% Map
    Mapped = [KeyValueMaker(X) || X <- List],
    [{SortedHKey, SortedHValue}|SortedT] = lists:keysort(1, Mapped),

    %% Reduce
    group_reduce(SortedT, SortedHKey, [SortedHValue]).


%% @doc Group pair values by key.
%% It is `map_group_with(fun(X) -> X end, List)'.
group_pairs([]) ->
    [];

group_pairs(List) ->
    [{SortedHKey, SortedHValue}|SortedT] = lists:keysort(1, List),
    group_reduce(SortedT, SortedHKey, [SortedHValue]).


%% @doc Calculates how many elements are with the same key.
%% It is the same as 
%%      `[{K, length(L)} || {K, L} <- group_with(KeyMaker, List)]'.
-spec group_count_with(fun(), list()) -> list({term(),non_neg_integer()}).

group_count_with(_keymaker, []) ->
    [];

group_count_with(KeyMaker, [_|_] = List) when is_function(KeyMaker, 1) ->
    %% Map
    Mapped = [KeyMaker(X) || X <- List],
    SortedMapped = lists:sort(Mapped),
    count_sorted_clusters(SortedMapped).

count_sorted_clusters([H|T]) ->
    count_sorted_clusters(T, H, 1);
count_sorted_clusters([]) ->
    [].

count_sorted_clusters([H|T], H, N) ->
    count_sorted_clusters(T, H, N+1);
count_sorted_clusters([H|T], K, N) ->
    [{K,N}|count_sorted_clusters(T, H, 1)];
count_sorted_clusters([], H, N) ->
    [{H,N}].




%% @doc Group tuples, using a value of field `N' as a key.
group_by(_N, []) ->
    [];

group_by(N, List) when is_integer(N), N > 0 ->
    %% Map
    [SortedH|SortedT] = lists:keysort(N, List),
    SortedHKey = element(N, SortedH),

    %% Reduce
    group_reduce(N, SortedT, SortedHKey, [SortedH]).
    

%% @doc Return `[{Key, [Value1, Value2, ...]}]'.
%% @end
%%
%% Still the same group:
%% group_reduce([{<<"user">>,{x_prefix_name,user,65,true,true,true}}],
%%                <<"author">>,[{x_prefix_name,author,65,true,false,true}])
%%
%% Version for ppairs.
group_reduce([{Key, Val}|T], Key, Vals) ->
    group_reduce(T, Key, [Val|Vals]);

%% Add a new group:
group_reduce([{NewKey, Val}|T], OldKey, Vals) ->
    [{OldKey, lists:reverse(Vals)} | group_reduce(T, NewKey, [Val])];

group_reduce([], Key, Vals) ->
    [{Key, lists:reverse(Vals)}].



%% @doc Genaralized version.
%%
%% @end
%% Still the same group:
group_reduce(N, [H|T], Key, Acc) when element(N, H) =:= Key ->
    group_reduce(N, T, Key, [H|Acc]);

%% Add a new group:
group_reduce(N, [H|T], Key, Acc) ->
    [{Key, lists:reverse(Acc)} | group_reduce(N, T, element(N, H), [H])];

group_reduce(_N, [], Key, Acc) ->
    [{Key, lists:reverse(Acc)}].
    

%% group_with end


%% @doc Apply `element(N, _)' for each element.
keys(N, List) ->
    [element(N, X) || X <- List].

elements(N, List) ->
    keys(N, List).


%% @doc This call is equal to 
%% `hd(lists:filter(F, L))'.
-spec filter_head(F, L) -> E when
    L :: [E],
    F :: fun((E) -> boolean()),
    E :: term().

filter_head(F, [H|T]) when is_function(F, 1) ->
    case F(H) of
        true -> H;
        false -> filter_head(F, T)
    end.


%% @doc Map with a counter.
%%
%% Call  `F(X, C)' for each element in `Xs', where `X' is an element and 
%% `C' is a counter from 1 to `length(Xs)'.
%% @end
cmap(F, Xs)  when is_function(F, 2), is_list(Xs) ->
    cmap(F, Xs, 1).


cmap(F, [X|Xs], C) ->
    [F(X, C)|cmap(F, Xs, C+1)];

cmap(_F, [], _C) ->
    [].


%% @doc Split a list into a list of sequencies using a key maker.
%% For example:
%%
%% ```
%% lists2:seq_group_with(fun(X) -> X rem 2 end, [2,2,4,1,3,4]).
%% [{0, [2,2,4]}, {1, [1,3]}, {0,[4]}
%% '''
seq_group_with(KeyMaker, [H|T]) when is_function(KeyMaker, 1) ->
    Key = KeyMaker(H),
    %% Acc stores elements of the group.
    Acc = [H],
    seq_group_with2(KeyMaker, T, Key, Acc);
seq_group_with(_KeyMaker, []) ->
    [].

seq_group_with2(KeyMaker, [H|T], Key, Acc) ->
    case KeyMaker(H) of
        Key -> 
            seq_group_with2(KeyMaker, T, Key, [H|Acc]);
        NewKey -> 
            [{Key, lists:reverse(Acc)} 
            | seq_group_with2(KeyMaker, T, NewKey, [H])]
    end;
seq_group_with2(_KeyMaker, [], Key, Acc) ->
    [{Key, lists:reverse(Acc)}].


%% @doc Collate in ascending order using a key maker.
collate_with(KeyMaker, List) when is_function(KeyMaker, 1), is_list(List) ->
    TaggedList = [{KeyMaker(X), X} || X <- List],
    SortedTaggedList = lists:keysort(1, TaggedList),
    keys(2, SortedTaggedList).
%   lists:sort(fun(X, Y) -> KeyMaker(X) < KeyMaker(Y) end, List).


%% @doc Collate in descending order using a key maker.
desc_collate_with(KeyMaker, List) when is_function(KeyMaker, 1), is_list(List) ->
    lists:sort(fun(X, Y) -> KeyMaker(X) > KeyMaker(Y) end, List).


%% @doc Convert a list of tuples to a tuple of lists.
-spec rotate(Tuples) -> Tuple when
    Tuples :: [tuple()| [tuple()]],
    Tuple :: tuple(list()).
rotate([X|_]=Xs) ->
    N = tuple_size(X),
    do_rotate(N, Xs, []).


do_rotate(0, _Tuples, Acc) ->
    list_to_tuple(Acc);
do_rotate(N, Tuples, Acc) ->
    Keys = keys(N, Tuples),
    do_rotate(N-1, Tuples, [Keys|Acc]).


-spec rotate(N, Tuples) -> Tuple when
    N :: non_neg_integer(),
    Tuples :: [tuple()],
    Tuple :: tuple(list()).

rotate(N, []) ->
    list_to_tuple(lists:duplicate(N, []));
rotate(N, Xs) ->
    do_rotate(N, Xs, []).




zip4([H1|T1], [H2|T2], [H3|T3], [H4|T4]) ->
    [{H1, H2, H3, H4}|zip4(T1, T2, T3, T4)];
zip4(_, _, _, _) ->
    [].


zip5([H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5]) ->
    [{H1, H2, H3, H4, H5}|zip5(T1, T2, T3, T4, T5)];
zip5(_, _, _, _, _) ->
    [].


zip6([H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5], [H6|T6]) ->
    [{H1, H2, H3, H4, H5, H6}|zip6(T1, T2, T3, T4, T5, T6)];
zip6(_, _, _, _, _, _) ->
    [].



zip_with4(F, [H1|T1], [H2|T2], [H3|T3], [H4|T4]) ->
    [F(H1, H2, H3, H4)|zip_with4(F, T1, T2, T3, T4)];
zip_with4(_, _, _, _, _) ->
    [].


zip_with5(F, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5]) ->
    [F(H1, H2, H3, H4, H5)|zip_with5(F, T1, T2, T3, T4, T5)];
zip_with5(_, _, _, _, _, _) ->
    [].


zip_with6(F, [H1|T1], [H2|T2], [H3|T3], [H4|T4], [H5|T5], [H6|T6]) ->
    [F(H1, H2, H3, H4, H5, H6)|zip_with6(F, T1, T2, T3, T4, T5, T6)];
zip_with6(_, _, _, _, _, _, _) ->
    [].


%% sorted_non_unique_elements([]) -> [a,d]
sorted_non_unique_elements(List) ->
    Sorted = lists:sort(List),
    sorted_to_non_unique_elements(Sorted).

-ifdef(TEST).

sorted_non_unique_elements_test_() ->
    [?_assertEqual([a,d], sorted_non_unique_elements([d,d,d,a,c,b,a])),
     ?_assertEqual([], sorted_non_unique_elements([])),
     ?_assertEqual([], sorted_non_unique_elements([1]))
    ].

-endif.

sorted_to_non_unique_elements([H,H|T]) ->
    %% H is not unique
    T2 = skip_matching_head(H, T),
    [H|sorted_to_non_unique_elements(T2)];
sorted_to_non_unique_elements([H|T]) ->
    %% H is unique
    sorted_to_non_unique_elements(T);
sorted_to_non_unique_elements([]) ->
    [].

sorted_unique_elements(List) ->
    Sorted = lists:sort(List),
    sorted_to_unique_elements(Sorted).

-ifdef(TEST).

sorted_unique_elements_test_() ->
    [?_assertEqual([b,c], sorted_unique_elements([d,d,d,a,c,b,a])),
     ?_assertEqual([], sorted_unique_elements([])),
     ?_assertEqual([1], sorted_unique_elements([1]))
    ].

-endif.

sorted_to_unique_elements([H,H|T]) ->
    %% H is not unique
    T2 = skip_matching_head(H, T),
    sorted_to_unique_elements(T2);
sorted_to_unique_elements([H|T]) ->
    %% H is unique
    [H|sorted_to_unique_elements(T)];
sorted_to_unique_elements([]) ->
    [].

skip_matching_head(H, [H|T]) ->
    skip_matching_head(H, T);
skip_matching_head(_, T) ->
    T.

-ifdef(TEST).

skip_matching_head_test_() ->
    [?_assertEqual([2,3], skip_matching_head(1, [1,2,3])),
     ?_assertEqual([f,d], skip_matching_head(a, [a,a,f,d])),
     ?_assertEqual([k,a,f], skip_matching_head(a, [k,a,f]))
    ].

-endif.

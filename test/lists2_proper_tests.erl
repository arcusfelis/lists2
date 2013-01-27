-module(lists2_proper_tests).
-compile([export_all]).


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


prop_group_count_with() ->
    ?FORALL(RandomList, list(),
        begin
        F = fun(X) -> X end, %% id
        equals(naive_group_count_with(F, RandomList), 
               lists2:group_count_with(F, RandomList))
        end).

naive_group_count_with(F, List) ->
    [{K, length(L)} || {K, L} <- lists2:group_with(F, List)].


%% -------------------------------------------------------------------
%% Property Testing
%% -------------------------------------------------------------------

run_property_testing_test() ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Res = proper:module(?MODULE, [{constraint_tries, 500}]),
    erlang:group_leader(EunitLeader, self()),
    ?assertEqual([], Res). 


group_with_test_() ->
    [?_assertEqual(lists2:group_with(fun(X) -> X rem 2 end, [1,2,4,5,3]),
                   [{0, [2, 4]}, {1, [1, 5, 3]}])
    ].

map_group_with_test_() ->
    [?_assertEqual(lists2:
                   map_group_with(fun(X) -> {X rem 2, integer_to_list(X)} end, 
                                  [1,2,4,5,3]),
                   [{0, ["2", "4"]}, {1, ["1", "5", "3"]}])
    ].

cmap_test_() ->
    [?_assertEqual(lists2:
                   cmap(fun(X, N) -> atom_to_list(X) ++ integer_to_list(N) end, 
                                  [a,b,c]),
                   ["a1", "b2", "c3"])
    ].

ordkeymerge_with_test_() ->
    F = fun lists2:ordkeymerge_with/4,
    L1 = [{1,a}, {2,b}],
    L2 = [{1,c}, {2,a}],
    L3 = [{2,c}, {3,a}],
    [?_assertEqual(F(1, fun zipper1/2, L1, L2), [{1,c}, {2,b}])
    ,?_assertEqual(F(1, fun zipper1/2, L1, L3), [{1,a}, {2,c}, {3,a}])
    ,?_assertEqual(F(1, fun zipper1/2, L3, L1), [{1,a}, {2,c}, {3,a}])
    ].

rotate_test_() ->
    F = fun lists2:rotate/1,
    [?_assertEqual(F([{1,a}, {2,b}, {3,c}]), {[1,2,3], [a,b,c]})
    ].


%% Return a record with a highest value in the second field.
zipper1(undefined, Y) -> Y;
zipper1(X, undefined) -> X;
zipper1(X, Y) when element(2, X) > element(2, Y) -> X;
zipper1(_, Y) -> Y.



group_pairs_test_() ->
    [?_assertEqual(lists2:group_pairs([]), [])
    ].

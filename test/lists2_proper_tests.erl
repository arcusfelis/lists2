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

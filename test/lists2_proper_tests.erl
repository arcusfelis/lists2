-module(lists2_proper_tests).
-compile([export_all]).


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% group_with start

%% Test the group_with function.
prop_group_with() ->
    ?FORALL(Result, non_empty_list({unique(term()), non_empty_list(term())}),
        begin
        %% Sorted by key, add the key as a part of the body
        SortedData    = lists:flatmap(fun flatten_prop_group_result/1, Result),
        ResultPlusKey = lists:map(fun result_plus_key/1, Result),
        RandomData = lists2:shuffle(SortedData),
        equals(lists2:group_with(fun key_maker_prop_group_with/1, RandomData), 
               ResultPlusKey)
        end).


unique(ElemTypes) ->
    ?LET(Values, list(ElemTypes), lists:usort(Values)).


%% @doc `KeyMaker' for `fun prop_group_with/0'.
key_maker_prop_group_with({Key, _Val}) ->
    Key.

flatten_prop_group_result({Key, Values}) ->
    [{Key, Value} || Value <- Values].


result_plus_key({Key, _Values} = Group) ->
    {Key, flatten_prop_group_result(Group)}.


%% group_with end
%% ------------------------------------------------------------------



%% -------------------------------------------------------------------
%% Property Testing
%% -------------------------------------------------------------------

run_property_testing_test() ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Res = proper:module(?MODULE, [{constraint_tries, 500}]),
    erlang:group_leader(EunitLeader, self()),
    ?assertEqual([], Res). 
